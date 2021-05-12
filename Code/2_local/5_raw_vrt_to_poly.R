# gc()
rm(list = ls())
# .rs.restartR()

library(smoothr)
library(raster)
library(stars)
library(sf)
library(tidyverse)
library(doParallel)
library(foreach)    

# my_brns_th <- 1.8
# my_ndwi_th <- 0.4

out_master_folder_full <- "C:/Users/bevin/Desktop/2020-06-17_clipToCol_30_50_210_260_smspline_06_18_04_005_001_005_001"
outfolder <- paste0(out_master_folder_full,"/2_smooth_polys_NAfill_raw/")

# 2779 7391 Are so small the script doesnt work

# LOAD BRNS
my_brns_full <- stars::read_stars(paste0(out_master_folder_full,"/0_gee_out_tif_tile_raw/brns3005.vrt"))
# st_crs(my_brns_full)

# LOAD NDWI
my_ndwi_full <- stars::read_stars(paste0(out_master_folder_full,"/0_gee_out_tif_tile_raw/ndwi3005.vrt"))
# st_crs(my_ndwi_full)

# LOAD GLACIERS
my_glaciers1985 <- read_sf("../../2018_RGI2/manuscript_outlines/Bolch/glacinvent_1985/bolch_1985_clean.shp") %>%  #_dissolve_pol_split
  mutate(n = row_number(), area_new = st_area(.))
# st_crs(my_glaciers1985)
st_crs(my_glaciers1985) <- st_crs(my_brns_full)

# my_time <- lubridate::now() %>% format("%Y%m%d_%H%M")

# my_glaciers1985 %>% filter(n == 0) %>% write_sf(paste0("2_Paper_BCA_Glacier/0_Data/Validation_Locations_Dissolve/out_all_brns_",my_time,".gpkg"))
# my_glaciers1985 %>% filter(n == 0) %>% write_sf(paste0("2_Paper_BCA_Glacier/0_Data/Validation_Locations_Dissolve/out_all_ndwi_",my_time,".gpkg"))
# outfolder <- paste0(out_master_folder_full,"/2_smooth_polys/")
dir.create(outfolder, showWarnings = F)
dir.create(paste0(outfolder,"/status_files"), showWarnings = F)

#### SELECTED GLACIER ####

all <- function(myname,
                # loess_th = my_loess_th,
                brns = my_brns_full,
                # brns_th = my_brns_th,
                ndwi = my_ndwi_full,
                # ndwi_th = my_ndwi_th,
                glaciers1985 = my_glaciers1985
                # time = my_time,
                ){
# myname = 1
  out_master_folder_full <- "C:/Users/bevin/Desktop/2020-06-17_clipToCol_30_50_210_260_smspline_06_18_04_005_001_005_001"
  outfolder <- paste0(out_master_folder_full,"/2_smooth_polys_NAfill_raw/")
  
  years = 1985:2019
  # myname=1
  # mygl <- my_glaciers1985[1,]
  # brns <- stars::read_stars(paste0(out_master_folder_full,"/1_gee_out_tif_tile_smooth/brns.vrt"))
  # ndwi <- stars::read_stars(paste0(out_master_folder_full,"/1_gee_out_tif_tile_smooth/ndwi.vrt"))
  # glaciers1985 <- read_sf("E:/Dropbox/FLNRO_p1/Research_Cryosphere/Project_Glacier/2018_RGI2/manuscript_outlines/Bolch/glacinvent_1985/bolch_1985_clean.shp") %>%  #_dissolve_pol_split
  #   mutate(n = row_number())
  # st_crs(glaciers1985) <- st_crs(brns)
  mygl <- glaciers1985[myname,]
  # myname=1
  
  # file.remove(paste0(outfolder, "/status_files/",myname,"_COMPLETE.txt"))
  file.remove(paste0(outfolder, "/status_files/COMPLETE_",myname,".txt"))
  
  
  source("2_Paper_BCA_Glacier/2_R_Code/2_local/STARS_GLACIER_FUNCTIONS.R")
  
  dir.create(paste0(outfolder, "/",myname), showWarnings = F)
  files <- list.files(paste0(outfolder, "/",myname), full.names = F, recursive = F)
  
  write.csv("",paste0(outfolder, "/status_files/1_clip_brns_",myname,".txt"))
  if(paste0(myname,"_brns.tif") %in% files){
    my_brns <- stars::read_stars(paste0(outfolder, "/",myname,"/",myname,"_brns.tif"))}else{
      my_brns <- brns[mygl] %>% st_as_stars()
      my_brns <- my_brns[,,,2:36]
      my_brns[my_brns<1.8] = 0
      my_brns[my_brns>=1.8] = 1
      write_stars(my_brns, paste0(outfolder, "/",myname,"/",myname,"_brns.tif"))}
  write.csv("",paste0(outfolder, "/status_files/2_clip_ndwi_",myname,".txt"))
  if(paste0(myname,"_ndwi.tif") %in% files){
    my_ndwi <- read_stars(paste0(outfolder, "/",myname,"/",myname,"_ndwi.tif"))}else{
      my_ndwi <- ndwi[mygl] %>% st_as_stars()
      my_ndwi <- my_ndwi[,,,2:36]
      # my_ndwi[my_ndwi==-9] = NA
      my_ndwi[my_ndwi<0.4] = 0
      my_ndwi[my_ndwi>=0.4] = 1      
      write_stars(my_ndwi, paste0(outfolder, "/",myname,"/",myname,"_ndwi.tif"))}
  
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
  # if(paste0(myname,"_rawPoly_brns_smooth_1.8.sqlite") %in% files){
    # my_brns_smooth_pol <- read_sf(paste0(outfolder, "/",myname,"/",myname,"_rawPoly_brns_smooth_1.8.sqlite"))}else{
      temp <- raster::stack(paste0(outfolder, "/",myname,"/",myname,"_brns.tif"))
      names(temp) <- years
      my_brns_smooth_pol <- do.call(rbind, lapply(years, rToP))
      # st_crs(my_brns_smooth_pol) <- st_crs(glaciers1985)
      write_sf(my_brns_smooth_pol, paste0(outfolder, "/",myname,"/",myname,"_rawPoly_brns_smooth_1.8.sqlite"))
      # }
  
  # write.csv("", paste0(outfolder, "/status_files/4_ndwi_to_poly_",myname,".txt"))
  # if(paste0(myname,"_rawPoly_ndwi_smooth_0.4.sqlite") %in% files){
    # my_ndwi_smooth_pol <- read_sf(paste0(outfolder, "/",myname,"/",myname,"_rawPoly_ndwi_smooth_0.4.sqlite"))}else{
      temp <- raster::stack(paste0(outfolder, "/",myname,"/",myname,"_ndwi.tif"))
      names(temp) <- years
      my_ndwi_smooth_pol <- do.call(rbind, lapply(years, rToP))
      # st_crs(my_ndwi_smooth_pol) <- st_crs(glaciers1985)
      write_sf(my_ndwi_smooth_pol, paste0(outfolder, "/",myname,"/",myname,"_rawPoly_ndwi_smooth_0.4.sqlite"))
      # }
  
    file.remove(paste0(outfolder, "/status_files/1_clip_brns_",myname,".txt"))
    file.remove(paste0(outfolder, "/status_files/2_clip_ndwi_",myname,".txt"))
    file.remove(paste0(outfolder, "/status_files/3_brns_to_poly_",myname,".txt"))
    file.remove(paste0(outfolder, "/status_files/4_ndwi_to_poly_",myname,".txt"))
  
    file.remove(paste0(outfolder, "/",myname,"/",myname,"_finalPoly_brns_smooth_1.8.sqlite"), layer = paste(myname,"brns",sep="_"))
    file.remove(paste0(outfolder, "/",myname,"/",myname,"_finalPoly_ndwi_smooth_1.8.sqlite"), layer = paste(myname,"brns",sep="_"))
    
  #### CLEAN WATER POLYGONS ####
  
    clean_all_year <- function(y){
      
      # if(paste0(myname,"_",y,"_finalPoly_ndwi_smooth_1.8.sqlite") %in% files){
        # return(y)}else{
      # y = 2015
      # my_brns_smooth_pol %>% plot()
      my_brns_smooth_pol_y <- my_brns_smooth_pol[my_brns_smooth_pol$year == y,] 
      my_ndwi_smooth_pol_y <- my_ndwi_smooth_pol[my_ndwi_smooth_pol$year == y,]
      
      write.csv("", paste0(outfolder, "/status_files/5_ndwi_clean_",myname,"_",y,".txt"))
      my_ndwi_smooth_pol_clean <- cleanerWater(my_ndwi_smooth_pol_y)
      my_ndwi_smooth_pol_clean <- my_ndwi_smooth_pol_clean %>% mutate(IceID = myname)
      
      #### CLEAN BR ####
      
      write.csv("", paste0(outfolder, "/status_files/6_brns_clean_",myname,"_",y,".txt"))
      my_brns_smooth_pol_clean <- cleanBR(ref_glacier = mygl, my_poly_to_clean = my_brns_smooth_pol_y)
      
      #### REMOVE WATER ####
      
      write.csv("", paste0(outfolder, "/status_files/7_brns_remove_water_",myname,"_",y,".txt"))
      my_brns_smooth_pol_clean_water <- do.call(rbind, lapply(y, removeWater, my_raster_brns = my_brns_smooth_pol_clean, my_raster_ndwi = my_ndwi_smooth_pol_clean))
      
      ### FINAL CLEAN ####
      
      write.csv("", paste0(outfolder, "/status_files/8_brns_final_clean_",myname,"_",y,".txt"))
      my_brns_smooth_pol_clean_water_final <- final_clean(my_brns_smooth_pol_clean_water)
      my_brns_smooth_pol_clean_water_final <- my_brns_smooth_pol_clean_water_final %>% mutate(IceID = myname)
      
      #### WRITE SF ####
      
      # write_sf(my_brns_smooth_pol_clean_water_final, paste0(outfolder, "/",myname,"/",myname,"_",y,"_finalPoly_brns_smooth_1.8.sqlite"))
      st_write(my_brns_smooth_pol_clean_water_final, paste0(outfolder, "/",myname,"/",myname,"_finalPoly_brns_smooth_1.8.sqlite"), layer = paste(myname,"brns",sep="_"), append=TRUE, quiet = T)
      
      # write_sf(my_ndwi_smooth_pol_clean, paste0(outfolder, "/",myname,"/",myname,"_",y,"_finalPoly_ndwi_smooth_1.8.sqlite"))
      st_write(my_ndwi_smooth_pol_clean, paste0(outfolder, "/",myname,"/",myname,"_finalPoly_ndwi_smooth_0.4.sqlite"), layer = paste(myname,"ndwi",sep="_"), append=TRUE, quiet = T)
      
      file.remove(paste0(outfolder, "/status_files/5_ndwi_clean_",myname,"_",y,".txt"))
      file.remove(paste0(outfolder, "/status_files/6_brns_clean_",myname,"_",y,".txt"))
      file.remove(paste0(outfolder, "/status_files/7_brns_remove_water_",myname,"_",y,".txt"))
      file.remove(paste0(outfolder, "/status_files/8_brns_final_clean_",myname,"_",y,".txt"))
      
      return(y)}
    # }
    
    l <- lapply(years,clean_all_year)
    rm(l)
      
    write.csv("", paste0(outfolder, "/status_files/COMPLETE_",myname,".txt"))
  
  # return(year)})
    
    
  return("done")
}

# all(1)
# all(myname, glaciers1985=my_glaciers1985)
# all(my_glaciers1985[2,])
#SLOW: 8428 (2015 212 mb - must be lots of polygons), 2779(no data), 7391 (no data)

# all(8245)
# all(3, brns = my_brns, ndwi = my_ndwi, glaciers1985 = my_glaciers1985)
# RUN FOR ALL GLACIERS 

# glids <- c(6,10,11,13,24,51,2779,4260,4282,7220,7391,7508)
# myname=1
# all(myname = 3,
#     # loess_th = my_loess_th, 
#     brns = my_brns_full, 
#     brns_th = my_brns_th,
#     ndwi = my_ndwi_full, 
#     ndwi_th = my_ndwi_th,
#     glaciers1985 = my_glaciers1985, 
#     # time = my_time,
#     years = 1984:2019)
# for(i in 1:5){all(i)}

cl <- parallel::makeCluster(parallel::detectCores())
doParallel::registerDoParallel(cl)
# all(my_glaciers1985[1,])
# foreach(i = glids) %do% print(i)
# df2$my[2:length(df2$my)]-1
x = foreach(i = 1:nrow(my_glaciers1985),
            .combine = 'rbind',
            .packages = c("stars","sf","dplyr","smoothr","raster"),
            .export = c("my_brns_full","my_ndwi_full","my_glaciers1985","all")
            ) %dopar% {
            all(myname = i,
                brns = my_brns_full,
                ndwi = my_ndwi_full,
                glaciers1985 = my_glaciers1985)
              # print(i)
              }

stopCluster(cl)


list <- list.files(outfolder, pattern = "_finalPoly_brns_smooth_1.8.sqlite", full.names = T, recursive = T)

my_read_write_sf <- function(index){
  print(index)
  my_sf <- read_sf(list[index]) %>% st_cast("MULTIPOLYGON")
  st_write(my_sf, paste0(outfolder,"finalPoly_brns_smooth_1.8_2.gpkg"), layer = "finalPoly_brns_smooth_1.8", append=TRUE, quiet = T)
  }

lapply(1:nrow(my_glaciers1985), my_read_write_sf)


list <- list.files(outfolder, pattern = "_finalPoly_ndwi_smooth_0.4.sqlite", full.names = T, recursive = T)

my_read_write_sf <- function(index){
  print(index)
  my_sf <- read_sf(list[index]) %>% st_cast("MULTIPOLYGON")
  st_write(my_sf, paste0(outfolder,"finalPoly_ndwi_smooth_0.4_2.gpkg"), layer = "finalPoly_ndwi_smooth_0.4", append=TRUE, quiet = T)
}

lapply(1:nrow(my_glaciers1985), my_read_write_sf)

# all(1)
# library(snowfall)
# sfInit(parallel = T, cpus = 48)
# # sfLibrary(smoothr)
# # sfLibrary(raster)
# # sfLibrary(stars)
# # sfLibrary(sf)
# # sfLibrary(dplyr)
# sfExport("out_master_folder_full","outfolder")
# out.lst <- sfClusterApplyLB(1:5, 
#                             fun = function(x) all(x))
# sfStop()



# library("doFuture")
# registerDoFuture()
# plan(multiprocess)
# foreach(i = 1:3) %dopar% {all(myname = i,
#                               brns = my_brns_full,
#                               ndwi = my_ndwi_full,
#                               glaciers1985 = my_glaciers1985)}
# 
# 
# cl <- parallel::makeCluster(parallel::detectCores())
# doParallel::registerDoParallel(cl)
# # all(my_glaciers1985[1,])
# # foreach(i = glids) %do% print(i)
# # df2$my[2:length(df2$my)]-1
# x = foreach(i = 1:10,#nrow(my_glaciers1985),
#             .combine = 'rbind',
#             .packages = c("stars","sf","dplyr","smoothr","raster"),
#             .export = c("my_brns_full","my_ndwi_full","my_glaciers1985","all")
# ) %dopar% {
#   all(myname = i,
#       brns = my_brns_full,
#       ndwi = my_ndwi_full,
#       glaciers1985 = my_glaciers1985)
#   # print(i)
# }
# 
# stopCluster(cl)
