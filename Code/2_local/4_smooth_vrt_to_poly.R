library(tidyverse)
library(smoothr)
library(stars)
library(sf)
library(tidyverse)
library(future.apply)
# library(foreach)
# library(doParallel)
# library(snowfall)

dat <- c("loess","moving","impute") # d = dat[1]


for(d in dat){

  # mystack_brns <- read_stars(paste0(out_master_folder_full,folder_smooth,"brns","_",d,"_th.vrt"), proxy = T)
  # mystack_ndwi <- read_stars(paste0(out_master_folder_full,folder_smooth,"ndwi","_",d,"_th.vrt"), proxy = T)
  
  plan(multisession, workers = parallel::detectCores())
  
  df <- future_lapply(1:nrow(glaciers1985), function(i){ # i=1

    write.csv("", paste0(out_master_folder_full,i, "_brns_",d,"_",brns_th*10,"_",ndwi_th*10,".txt"))
    
    myglacier <- st_read("Data/glaciers_n.sqlite", query = paste0("SELECT * FROM glaciers_n WHERE n=",i)) %>% st_transform(3005)
    
    file.rename(from = paste0(out_master_folder_full,i,"_brns_",d,"_",brns_th*10,"_",ndwi_th*10,".txt"),
                to = paste0(out_master_folder_full,i,"_brns_",d,"_",brns_th*10,"_",ndwi_th*10,"glacier.txt"))
    
    sf::gdal_utils(util = "buildvrt",
                   source = paste0(out_master_folder_full,folder_smooth,"brns","_",d,"_th.vrt"),
                   destination = paste0(out_master_folder_full,i,"_brns_",d,"_",brns_th*10,"_",ndwi_th*10,".vrt"),
                   options = c("-te", st_bbox(myglacier)))
    sf::gdal_utils(util = "buildvrt",
                   source = paste0(out_master_folder_full,folder_smooth,"ndwi","_",d,"_th.vrt"),
                   destination = paste0(out_master_folder_full,i,"_ndwi_",d,"_",brns_th*10,"_",ndwi_th*10,".vrt"),
                   options = c("-te", st_bbox(myglacier)))

    file.rename(from = paste0(out_master_folder_full,i,"_brns_",d,"_",brns_th*10,"_",ndwi_th*10,"glacier.txt"),
                to = paste0(out_master_folder_full,i,"_brns_",d,"_",brns_th*10,"_",ndwi_th*10,"readingvrt.txt"))

    mystack_brns <- stars::read_stars(paste0(out_master_folder_full,i,"_brns_",d,"_",brns_th*10,"_",ndwi_th*10,".vrt"), proxy = T)
    mystack_ndwi <- stars::read_stars(paste0(out_master_folder_full,i,"_ndwi_",d,"_",brns_th*10,"_",ndwi_th*10,".vrt"), proxy = T)
    
    file.rename(from = paste0(out_master_folder_full,i,"_brns_",d,"_",brns_th*10,"_",ndwi_th*10,"readingvrt.txt"),
                to = paste0(out_master_folder_full,i,"_brns_",d,"_",brns_th*10,"_",ndwi_th*10,"loaded.txt"))
    
    # Clip stacks to glacier
    mystack_brns_clip <- mystack_brns[myglacier] %>% st_as_stars()
    mystack_ndwi_clip <- mystack_ndwi[myglacier] %>% st_as_stars()
    
    file.rename(from = paste0(out_master_folder_full,i,"_brns_",d,"_",brns_th*10,"_",ndwi_th*10,"loaded.txt"),
                to = paste0(out_master_folder_full,i,"_brns_",d,"_",brns_th*10,"_",ndwi_th*10,"clipped.txt"))
    
    # Stack to Polygon ####
    stack_to_pol <- function(j, th, temp_ras){
      
      # To polygon j=38
      temp_pol <- (temp_ras[,,,j]) %>% st_as_sf(as_points = FALSE, merge = T) 
      names(temp_pol) <- c("val","geometry")
      
      # FIX EDGE EFFECTS OF PIXELS
      temp_pol <- temp_pol %>% 
        filter(val != 0) %>% 
        select(-val) %>%
        st_make_valid() %>% 
        st_intersection(myglacier) %>% 
        select("geometry")
      
      if(nrow(temp_pol)>0){
        sym_dif <- temp_pol %>% 
          st_sym_difference(myglacier %>% st_transform(st_crs(temp_pol)) %>% select(n)) 
        littlepieces <- lapply(1:nrow(sym_dif), function(i) {
          st_cast(sym_dif[i, ], "POLYGON")}) %>%
          do.call(rbind, .)  %>% 
          # st_cast("POLYGON") %>% mutate(n = row_number())
          mutate(area = as.numeric(st_area(.))/(1000*1000)) %>%
          filter(area < 0.002) %>% 
          select(-area)
        temp_pol_new <- 
          bind_rows(temp_pol, littlepieces) %>% mutate(year = years[j]) %>%       
          st_cast("POLYGON") %>% 
          summarise() %>% 
          mutate(year = years[j]) %>% 
          mutate(area = as.numeric(st_area(.))/(1000*1000))
          }else{temp_pol_new=temp_pol}
      
      return(temp_pol_new)}
    
    pol_brns <- do.call(bind_rows, lapply(1:length(years), FUN = stack_to_pol, th = brns_th, temp_ras = mystack_brns_clip))
    pol_ndwi <- do.call(bind_rows, lapply(1:length(years), FUN = stack_to_pol, th = ndwi_th, temp_ras = mystack_ndwi_clip))
  
    file.rename(from = paste0(out_master_folder_full,i,"_brns_",d,"_",brns_th*10,"_",ndwi_th*10,"clipped.txt"),
                to = paste0(out_master_folder_full,i,"_brns_",d,"_",brns_th*10,"_",ndwi_th*10,"edge.txt"))
    
  # Clean water ####
    
  pol_brns <- pol_brns%>% mutate(area_water_km2 = 0)
  if(nrow(pol_ndwi)>0){
    
    pol_ndwi_clean <- pol_ndwi %>% 
      st_cast("MULTIPOLYGON") %>%
      st_cast("POLYGON") %>% 
      fill_holes(threshold = units::set_units(ndwi_fill_th, km^2)) %>%
      # drop_crumbs(threshold = units::set_units(ndwi_area_th, km^2)) %>%
      mutate(area_km2 = as.numeric(st_area(.))/(1000*1000), n = 1) %>% 
      filter(area_km2 >= ndwi_area_th) %>% 
      group_by(year) %>% 
      summarise()
    
    pol_ndwi_clean %>% st_write(paste0(out_master_folder_full,folder_poly,i,
                                             "_brns_",d,"_",brns_th*10,"_",ndwi_th*10,"_ndwi.gpkg"))
    
  # Remove water from band ratio ####
    
    pol_brns <- do.call(bind_rows, lapply(years, function(y){
      # print(y)
      temp_ndwi <- pol_ndwi_clean %>% filter(year == y) %>% select(-year)
      temp_brns <- pol_brns %>% filter(year == y)
      if(nrow(temp_ndwi)>0){
      clean_water <- st_difference(temp_brns,temp_ndwi) %>% mutate(area_water_km2 = as.numeric(st_area(temp_ndwi))/(1000*1000))
      return(clean_water)}else{
      return(temp_brns)
      }}))
    }
    
    file.rename(from = paste0(out_master_folder_full,i,"_brns_",d,"_",brns_th*10,"_",ndwi_th*10,"edge.txt"),
                to = paste0(out_master_folder_full,i,"_brns_",d,"_",brns_th*10,"_",ndwi_th*10,"cleanpol.txt"))
    
    # Clean and summarize band ratio
    
    pol_brns_clean_final <-
      pol_brns %>%
      #SPLIT
      st_cast("MULTIPOLYGON") %>% 
      st_cast("POLYGON") %>% 
      #FILL 
      fill_holes(threshold = units::set_units(brns_area_th, km^2)) %>% 
      # DROP FAST
      mutate(area_km2 = as.numeric(st_area(.))/(1000*1000), n = 1) %>% 
       filter(area_km2 >= brns_area_th) %>% 
      #DISSOLVE
      ungroup() %>%
      group_by(year) %>% 
      summarise(
        area_water_km2 = sum(area_water_km2),
        nparts = sum(n)) %>% 
      #AREA 
      mutate(area_glacier_km2 = as.numeric(st_area(.))/(1000*1000),
             year = as.numeric(year),
             n = myglacier$n)
    
    file.remove(paste0(out_master_folder_full,i, "_brns_",d,"_",brns_th*10,"_",ndwi_th*10,"cleanpol.txt"))
    
    pol_brns_clean_final %>% st_write(paste0(out_master_folder_full,folder_poly,i,
                                             "_brns_",d,"_",brns_th*10,"_",ndwi_th*10,".gpkg"))
    
    file.remove(paste0(out_master_folder_full,i,"_brns_",d,"_",brns_th*10,"_",ndwi_th*10,".vrt"))
    file.remove(paste0(out_master_folder_full,i,"_ndwi_",d,"_",brns_th*10,"_",ndwi_th*10,".vrt"))
    
    
  return(i)
  }
  )
  
  future:::ClusterRegistry("stop")
  
  }
