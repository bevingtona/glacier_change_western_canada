rm(list=ls())
#### LOAD LIBRARIES #### 

library(sf)
library(tidyverse)
library(patchwork)
library(ggrepel)
library(bcmaps)
library(DBI)
library(RSQLite)
library(future.apply)

# ROOT FOLDER #### 

  # folder <- "E:/Google Drive/GlacierChangeBC_Paper_rgee_2020_iter_ct4_wholeRegion/"
  # folder <- "E:/Google Drive/GlacierChangeBC_Paper_rgee_2020_iter_ct4_wholeRegion/"
  folder <- "E:/Google Drive/GlacierChangeBC_Paper_rgee_2021/"
  
# REGIONS ####

  regions <- read_sf("../../2018_RGI2/manuscript_outlines/Bolch/glacinvent_regions/bca_glacier_regions.shp") %>% 
    rename(region = Constrain)

# TOBIAS GLACIERS ####
  
  gl_1985_2005 <- st_read("Data/gl_1985_2005.gpkg")
  
  gl_1985_2005 %>% 
    st_drop_geometry() %>% 
    summarize(sum1985 = sum(area_1985_km2, na.rm = T),
              count1985 = sum(!is.na(area_1985_km2)),
              sum1985 = sum(area_1985_km2, na.rm = T),
              sum2005 = sum(area_2005_km2, na.rm = T),
              count2005 = sum(!is.na(area_2005_km2)))
  
  # #### LOAD 1985 ####
  # gl_1985 <- read_sf("../../2018_RGI2/manuscript_outlines/Bolch/glacinvent_1985/bolch_1985_clean.shp") %>%
  #   mutate(region = case_when(region == "NCMn" ~ "NCM", region == "SCMs" ~ "SCM", T ~ region)) %>%
  #   mutate(area_1985_km2 = as.numeric(st_area(.))/(1000*1000)) %>%
  #   mutate(n = row_number())
  # 
  # #### LOAD 2005 ####
  # gl_2005 <- read_sf("../../2018_RGI2/manuscript_outlines/Bolch/glacinvent_2005/glacinvent_2005_poly_final.shp") %>%
  #   dplyr::select(geometry)
  #  
  # #### IN PARALLEL! PER GLACIER JOIN 2005 to 1985 #### 
  # plan(multisession)
  # gl_1985_2005 <- do.call(bind_rows,
  #                         future_lapply(1:14411, function(i){
  #                           my_1985_gl <- gl_1985 %>% filter(n == i)
  #                           my_2005_crop <- gl_2005 %>% st_crop(my_1985_gl)
  #                           my_2005_clip <- my_2005_crop %>%
  #                             st_intersection(my_1985_gl %>% dplyr::select(n)) %>%
  #                             group_by(n) %>% 
  #                             summarize() %>% 
  #                             mutate(area_2005_km2 = as.numeric(st_area(.))/(1000*1000)) %>% 
  #                             st_drop_geometry()
  #   return(full_join(my_1985_gl, my_2005_clip))}))
  #
  # #### WRITE RESULT ####
  # st_write(gl_1985_2005, "E:/Google Drive/GlacierChangeBC_Paper_rgee_2020_iter_ct4_wholeRegion/gl_1985_2005.gpkg")
  
# LOAD DATA #### 
  
  #### IMPUTE #### 20 s
  lyr <- paste0(folder,"impute_tonight.gpkg")
  mydb <- dbConnect(RSQLite::SQLite(), lyr)
  system.time(dat <- dbGetQuery(mydb, "SELECT * FROM glacier"))
  system.time(impute <- dat %>% st_as_sf())
  dbDisconnect()
  
  #### MOVING #### 20 s
  lyr <- paste0(folder,"moving_tonight.gpkg")
  mydb <- dbConnect(RSQLite::SQLite(), lyr)
  system.time(dat <- dbGetQuery(mydb, "SELECT * FROM glacier"))
  system.time(moving <- dat %>% st_as_sf())
  dbDisconnect()
  
  #### LOESS #### 20 s
  lyr <- paste0(folder,"loess_tonight.gpkg")
  mydb <- dbConnect(RSQLite::SQLite(), lyr)
  system.time(dat <- dbGetQuery(mydb, "SELECT * FROM glacier"))
  system.time(loess <- dat %>% st_as_sf())
  dbDisconnect()
  
# ALL POLYGONS ####  

  #### BIND DATASETS ####
  new_all <- bind_rows(impute %>% mutate(method = "impute"), 
                       moving %>% mutate(method = "moving"), 
                       loess %>% mutate(method = "loess"))

# CLEAN POLYGONS #### 
  
  #### JOIN TO 1985 #### 40 secs
  new_all_join <- full_join(new_all, 
                            gl_1985_2005 %>% st_drop_geometry() %>% 
                              dplyr::select(n, name, region, PROV, 
                                     area_1985_km2, 
                                     area_2005_km2))
  new_all_join <- new_all_join %>% filter(!is.na(method))
  
  #### AREA DIFF FROM 1985 ####
  new_all_join <- new_all_join  %>% 
    mutate(area_gl_diff_1985_km2 = area_glacier_km2  - area_1985_km2, 
           area_gl_diff_1985_perc = (100*(area_glacier_km2/area_1985_km2))-100,
           area_gl_diff_2005_km2 = area_glacier_km2  - area_2005_km2, 
           area_gl_diff_2005_perc = (100*(area_glacier_km2/area_2005_km2))-100) 
    
  #### LAKE TERMINATING ID mean water area > 0.05 for 2015-2020 ####
  water_id <- new_all_join %>% st_drop_geometry() %>%
      filter(year > 2015) %>% 
      group_by(method, n) %>% 
      summarize(mean = mean(area_water_km2, na.rm = T))%>% # Number of years where there is water, per method/glacier
      mutate(wateryn = mean>0.01) %>%  # Water Area Threshold
      group_by(method, n) %>% 
      summarize(sum = sum(wateryn)) %>% # Number of years where there is water, per method/glacier
      group_by(n) %>% 
      summarize(sum = sum(sum)) %>% # Sum of years where there is water, per method/glacier
      mutate(wateryn = sum >=1) %>% # Average number of years with water, per glacier
      mutate(wateryn = case_when(wateryn == T ~ 1, TRUE ~ 0)) %>% 
      dplyr::select(-sum) 
  water_id %>% group_by(wateryn) %>% summarize(n = n())
    
  #### DEBRIS COVER ID > 5% diff in 85 and 05####
  
  debris_id <-
    full_join(
    new_all_join %>% st_drop_geometry() %>% filter(year == 1985, area_1985_km2 >= 1) %>% 
      group_by(n) %>% summarize(debris_1985 = mean(area_gl_diff_1985_perc, na.rm = T)),
    new_all_join %>% st_drop_geometry() %>% filter(year == 2005) %>% 
      group_by(n) %>% summarize(debris_2005 = mean(area_gl_diff_2005_perc, na.rm = T))) %>%  
    mutate(debrisyn = (debris_1985<(-10)) + (debris_2005<(-10))) %>% 
    dplyr::select(n, debrisyn) %>% 
    mutate(debrisyn = case_when(debrisyn ==  2 ~ 1, TRUE ~ 0))
  
  debris_id %>% group_by(debrisyn) %>% summarize(n = n())
  
  #### JOIN ####
  
  # gl_1985_2005_id <- full_join(gl_1985_2005, full_join(water_id, debris_id))
  # file.remove(paste0(folder,"gl_1985_2005_id.gpkg"))
  # gl_1985_2005_id %>% st_write(paste0(folder,"gl_1985_2005_id.gpkg"))
  
  new_all_join_id <- full_join(new_all_join, 
                       full_join(water_id, debris_id))
  
  new_all_join_id <- new_all_join_id %>% 
    mutate(type_long = case_when(wateryn == 1 & debrisyn == 0 ~ "Proglacial Lake Glacier",
                                 wateryn == 0 & debrisyn == 1 ~ "Debris-Covered Glacier",
                                 wateryn == 1 & debrisyn == 1 ~ "Debris-Covered Glacier",
                                 T ~ "Clean Ice Glacier")) %>% 
    mutate(type_short = case_when(type_long == "Proglacial Lake Glacier" ~ "PLG",
                                  type_long == "Debris-Covered Glacier" ~ "DCG",
                                  type_long == "Clean Ice Glacier" ~ "CIG",
                                  T ~ "No type"))
  
  new_all_join_id <- new_all_join_id %>% 
    mutate(area_1985_km2_group = case_when(area_1985_km2 < 1 ~ "<1 sq. km", 
                                           area_1985_km2 >= 1 ~ ">= 1 sq. km", 
                                           T ~ "other"))

#### Clean workspace ####
  
  rm(debris_id, water_id, new_all_join, new_all, impute, loess, moving, mydb, dat, lyr)
  
  st_crs(new_all_join_id) <- 3005
  
  # new_all_join_id <- new_all_join_id %>% 
  #   filter(row_number()!=c(525007,532463)) 
  # 
  # new_all_join_id <- new_all_join_id %>% st_make_valid()
  # 
  # st_write(new_all_join_id, "Data_2021/1984_2021_new_all_join_id.gpkg")
  
  