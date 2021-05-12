library(tidyverse)
library(smoothr)
library(raster)
library(stars)
library(sf)
library(tidyverse)
library(doParallel)
library(foreach)    

# 2779 7391 Are so small the script doesnt work

fol <- "2_smooth_polys_NAfill"
dir.create(paste0(out_master_folder_full,"/",fol), showWarnings = F)
outfolder <- paste0(out_master_folder_full,"/",fol,"/",brns_name)

# LOAD BRNS
my_brns_full <- stars::read_stars(paste0(out_master_folder_full,"/1_gee_out_tif_tile_smooth_NAfill/",brns_name,".vrt"))
st_crs(my_brns_full)

# LOAD NDWI
my_ndwi_full <- stars::read_stars(paste0(out_master_folder_full,"/1_gee_out_tif_tile_smooth_NAfill/",ndwi_name,".vrt"))
st_crs(my_ndwi_full)

# LOAD GLACIERS
my_glaciers1985 <- read_sf("../../2018_RGI2/manuscript_outlines/Bolch/glacinvent_1985/bolch_1985_clean.shp") %>%  #_dissolve_pol_split
  mutate(n = row_number(), area_new = st_area(.))
st_crs(my_glaciers1985) <- st_crs(my_brns_full)
# write_sf(my_glaciers1985,"bolch_1985_clean_n.sqlite")
dir.create(outfolder, showWarnings = F)
dir.create(paste0(outfolder,"/status_files"), showWarnings = F)

#### SELECTED GLACIER ####

all <- function(myname=1,
                # loess_th = my_loess_th,
                brns = my_brns_full,
                # brns_th = my_brns_th,
                ndwi = my_ndwi_full,
                # ndwi_th = my_ndwi_th,
                glaciers1985 = my_glaciers1985, 
                brns_name = brns_name, 
                ndwi_name = ndwi_name
                # time = my_time,
                ){

  years = 1984:2020
  # myname = 5
  mygl <- glaciers1985[myname,]
  
  file.remove(paste0(outfolder, "/status_files/COMPLETE_",myname,".txt"))
  
  source("2_Paper_BCA_Glacier/2_R_Code/2_local/STARS_GLACIER_FUNCTIONS.R")
  
  dir.create(paste0(outfolder, "/",myname), showWarnings = F)
  files <- list.files(paste0(outfolder, "/",myname), full.names = F, recursive = F)
  
  write.csv("",paste0(outfolder, "/status_files/1_clip_brns_",myname,".txt"))
  if(paste0(myname,"_brns.tif") %in% files){
  my_brns <- stars::read_stars(paste0(outfolder, "/",myname,"/",myname,"_brns.tif"))}else{
      my_brns <- brns[mygl]
      # st_crs(my_brns) <- 3005
      my_brns <- st_as_stars(my_brns)
      write_stars(my_brns, paste0(outfolder, "/",myname,"/",myname,"_brns.tif"), type = "Byte", NA_value = NA)}
  file.remove(paste0(outfolder, "/status_files/1_clip_brns_",myname,".txt"))
  
  write.csv("",paste0(outfolder, "/status_files/2_clip_ndwi_",myname,".txt"))
  if(paste0(myname,"_ndwi.tif") %in% files){
  my_ndwi <- read_stars(paste0(outfolder, "/",myname,"/",myname,"_ndwi.tif"))}else{
      my_ndwi <- ndwi[mygl]
      my_ndwi <- st_as_stars(my_ndwi)
      # st_crs(my_ndwi) <- 3005
      write_stars(my_ndwi, paste0(outfolder, "/",myname,"/",myname,"_ndwi.tif"))}
  file.remove(paste0(outfolder, "/status_files/2_clip_ndwi_",myname,".txt"))
  
  # f <- loess_th
  
  rToP <- function(r){
    r <- temp[[paste0("X",r)]]
    s <- stars::st_as_stars(r>0)
    st_crs(s) <- 3005
    # plot(r)
    s <- sf::st_as_sf(s, as_points = FALSE, merge = T)
    names(s) <- c("val","geometry")
    s <- s %>%
      group_by(val) %>%
      # filter(val == 1) %>% 
      mutate(year = as.numeric(sub("X","",names(r))))
    # plot(s)
    return(s)}
  
  write.csv("", paste0(outfolder, "/status_files/3_brns_to_poly_",myname,".txt"))
  if(paste0(myname,"_2020_glacier_",brns_name,"_",ndwi_name,".sqlite") %in% files){
    file.remove(paste0(outfolder, "/status_files/3_brns_to_poly_",myname,".txt"))}else{
      temp <- raster::stack(paste0(outfolder, "/",myname,"/",myname,"_brns.tif"))
      names(temp) <- years
      my_brns_smooth_pol <- do.call(rbind, lapply(years, rToP))
      file.remove(paste0(outfolder, "/status_files/3_brns_to_poly_",myname,".txt"))
      }
  write.csv("", paste0(outfolder, "/status_files/4_ndwi_to_poly_",myname,".txt"))
  if(paste0(myname,"_2020_water_",brns_name,"_",ndwi_name,".sqlite") %in% files){
  file.remove(paste0(outfolder, "/status_files/4_ndwi_to_poly_",myname,".txt"))}else{
      temp <- raster::stack(paste0(outfolder, "/",myname,"/",myname,"_ndwi.tif"))
      names(temp) <- years
      my_ndwi_smooth_pol <- do.call(rbind, lapply(years, rToP))
      file.remove(paste0(outfolder, "/status_files/4_ndwi_to_poly_",myname,".txt"))
      }
  
    
  
    # file.remove(paste0(outfolder, "/",myname,"/",myname,"_finalPoly_brns_smooth_1.8.sqlite"), layer = paste(myname,"brns",sep="_"))
    # file.remove(paste0(outfolder, "/",myname,"/",myname,"_finalPoly_ndwi_smooth_1.8.sqlite"), layer = paste(myname,"brns",sep="_"))
    
  #### CLEAN WATER POLYGONS ####
  
    clean_all_year <- function(y){
    # y=2000
      write.csv("", paste0(outfolder, "/status_files/5_",myname,"_",y,".txt"))
      
      # if(paste0(myname,"_",y,"_glacier_",brns_name,"_",ndwi_name,".sqlite") %in% files){
      #   file.remove(paste0(outfolder, "/status_files/5_",myname,"_",y,".txt"))
      # #   
      #   return(y)}else{
        # y = 2002
        # my_brns_smooth_pol %>% plot()
        my_brns_smooth_pol_y <- my_brns_smooth_pol[my_brns_smooth_pol$year == y,] 
        my_ndwi_smooth_pol_y <- my_ndwi_smooth_pol[my_ndwi_smooth_pol$year == y,]
        
        # write.csv("", paste0(outfolder, "/status_files/5_ndwi_clean_",myname,"_",y,".txt"))
        my_ndwi_smooth_pol_clean <- cleanerWater(my_ndwi_smooth_pol_y)
        my_ndwi_smooth_pol_clean <- my_ndwi_smooth_pol_clean %>% mutate(IceID = myname)
        
        #### CLEAN BR ####
        
        # write.csv("", paste0(outfolder, "/status_files/6_brns_clean_",myname,"_",y,".txt"))
        my_brns_smooth_pol_clean <- cleanBR(ref_glacier = mygl, my_poly_to_clean = my_brns_smooth_pol_y)
        
        #### REMOVE WATER ####
        
        # write.csv("", paste0(outfolder, "/status_files/7_brns_remove_water_",myname,"_",y,".txt"))
        my_brns_smooth_pol_clean_water <- do.call(rbind, lapply(y, removeWater, my_raster_brns = my_brns_smooth_pol_clean, my_raster_ndwi = my_ndwi_smooth_pol_clean))
        
        ### FINAL CLEAN ####
        
        # write.csv("", paste0(outfolder, "/status_files/8_brns_final_clean_",myname,"_",y,".txt"))
        my_brns_smooth_pol_clean_water_final <- final_clean(my_brns_smooth_pol_clean_water)
        my_brns_smooth_pol_clean_water_final <- my_brns_smooth_pol_clean_water_final %>% mutate(IceID = myname)
        
        #### WRITE SF ####
        
        write_sf(my_brns_smooth_pol_clean_water_final, paste0(outfolder, "/",myname,"/",myname,"_",y,"_glacier_",brns_name,"_",ndwi_name,".sqlite"))
        # st_write(my_brns_smooth_pol_clean_water_final, paste0(outfolder, "/",myname,"/",myname,"_finalPoly_brns_smooth_1.8.sqlite"), layer = paste(myname,"brns",sep="_"), append=TRUE, quiet = T)
        
        # my_sf <-  #%>% mutate(area_km2 = round(area_km2, 4))
        # st_write(my_brns_smooth_pol_clean_water_final %>% st_cast("MULTIPOLYGON"), paste0(outfolder,"glaciers_",brns_name,"_",ndwi_name,"_v2.gpkg"), layer = paste0(brns_name,"_",ndwi_name), append=TRUE, quiet = T)
        
        write_sf(my_ndwi_smooth_pol_clean, paste0(outfolder, "/",myname,"/",myname,"_",y,"_water_",brns_name,"_",ndwi_name,".sqlite"))
        # st_write(my_ndwi_smooth_pol_clean, paste0(outfolder, "/",myname,"/",myname,"_finalPoly_ndwi_smooth_0.4.sqlite"), layer = paste(myname,"ndwi",sep="_"), append=TRUE, quiet = T)
    
        # my_sf <-  #%>% mutate(area_km2 = round(area_km2, 4))
        # st_write(my_ndwi_smooth_pol_clean %>% st_cast("MULTIPOLYGON"), paste0(outfolder,"water_",brns_name,"_",ndwi_name,"_v2.gpkg"), layer = paste0(brns_name,"_",ndwi_name), append=TRUE, quiet = T)
        
        # file.remove(paste0(outfolder, "/status_files/5_ndwi_clean_",myname,"_",y,".txt"))
        # file.remove(paste0(outfolder, "/status_files/6_brns_clean_",myname,"_",y,".txt"))
        # file.remove(paste0(outfolder, "/status_files/7_brns_remove_water_",myname,"_",y,".txt"))
        # file.remove(paste0(outfolder, "/status_files/8_brns_final_clean_",myname,"_",y,".txt"))
        file.remove(paste0(outfolder, "/status_files/5_",myname,"_",y,".txt"))
        
      return(y)}
    
    # }
    # }
    
    l <- lapply(years,clean_all_year)
    rm(l)
      
    write.csv("", paste0(outfolder, "/status_files/COMPLETE_",myname,".txt"))
  
  # return(year)})
    
    
  return("done")
  }
# all(5)

#### RUN PER GLACIER PARALLELL ####

  cl <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
  
  x = foreach(i = 1:nrow(my_glaciers1985),
              .combine = 'rbind',
              .packages = c("stars","sf","dplyr","smoothr","raster"),
              .export = c("my_brns_full","my_ndwi_full","my_glaciers1985","all","out_master_folder_full","outfolder","brns_name","ndwi_name")
              ) %dopar% {
              all(myname = i,
                  brns = my_brns_full,
                  ndwi = my_ndwi_full,
                  glaciers1985 = my_glaciers1985,
                  brns_name = brns_name, 
                  ndwi_name = ndwi_name)}
  
  stopCluster(cl)


list <- list.files(outfolder, pattern = paste0("_glacier_",brns_name,"_",ndwi_name,".sqlite"), full.names = T, recursive = T)

my_read_write_sf <- function(index){
  print(index)
  my_sf <- read_sf(list[index]) %>% st_cast("MULTIPOLYGON")
  st_write(my_sf, 
           paste0(outfolder,"/glacier_",brns_name,"_",ndwi_name,".gpkg"), 
           layer = paste0("glacier_",brns_name,"_",ndwi_name), append=TRUE, quiet = T)}

lapply(1:length(list), my_read_write_sf)

list <- list.files(outfolder, pattern = paste0("_water_",brns_name,"_",ndwi_name,".sqlite"), full.names = T, recursive = T)

my_read_write_sf <- function(index){
  print(index)
  my_sf <- read_sf(list[index]) %>% st_cast("MULTIPOLYGON")
  st_write(my_sf, 
           paste0(outfolder,"/water_",brns_name,"_",ndwi_name,".gpkg"), 
           layer = paste0("water_",brns_name,"_",ndwi_name), append=TRUE, quiet = T)}

lapply(1:length(list), my_read_write_sf)


