# rm(list=ls())
library(mapview)
library(smoothr)
library(raster)
library(stars)
library(sf)
library(tidyverse)
library(doParallel)
library(foreach)
library(bcdata)
library(dplyr)

dir.create(paste0(out_master_folder_full,"/",dirrr,"/"), showWarnings = F)
dir.create(paste0(out_master_folder_full,"/",dirrr,"/brns/"), showWarnings = F)
dir.create(paste0(out_master_folder_full,"/",dirrr,"/ndwi/"), showWarnings = F)
                                 
# FOREACH TILE SMOOTH

cl <- parallel::makeCluster(parallel::detectCores())
doParallel::registerDoParallel(cl)

x = foreach(p = 1:nrow(polys),
            .combine = 'rbind',
            .packages = c("stars","sf","dplyr","smoothr","raster","rms","pspline"),
            .export = c("polys","out_master_folder_full","infolder","dirrr")) %dopar% {
# for(p in 1:nrow(polys)){
  my_brns <- stars::read_stars(paste0(out_master_folder_full,"/",infolder,"/brns3005.vrt"))
  my_ndwi <- stars::read_stars(paste0(out_master_folder_full,"/",infolder,"/ndwi3005.vrt"))
  
#### BRNS ####
  
  brns_th <-  18              
  
  # List files for tile ####
    
    brns_files <- list.files(
      path = paste0(out_master_folder_full,"/",dirrr,"/brns/"), 
      pattern = paste0("^",p,"_"))
    write.csv("", paste0(out_master_folder_full,"/",dirrr,"/",p,".txt"))            
    
  # Export raw raster ####
    
    raw_name <- paste0(p,"_brns.tif")
    # if(raw_name %in% brns_files){
    # 
    #   # READ IF EXISTS
    # 
    #   my_brns_tile <- read_stars(paste0(out_master_folder_full,"/",dirrr,"/brns/",raw_name))
    # 
    # }else{
      
      # CLIP 
      
      my_brns_tile <- my_brns[polys[p,]] %>% st_as_stars()
      
      # REMOVE NA 
      
      my_brns_tile[my_brns_tile==-9] = NA
      my_brns_tile[my_brns_tile<0] = NA
      
      # WRITE 
      
      write_stars(my_brns_tile, paste0(out_master_folder_full,"/",dirrr,"/brns/",raw_name))
    # }

  # Export imputed raster ####
    
    impute_name <- paste0(p,"_brns_impute.tif")
    
    # if(impute_name %in% brns_files){
    # 
    #   # READ IF EXISTS
    # 
    #   my_brns_tile_impute <- read_stars(paste0(out_master_folder_full,"/",dirrr,"/brns/",impute_name))
    # 
    # }else{
      
      # IMPUTE FUNCTION 
      
      my_func_impute <- function(y,x) {
        df <- data.frame(xx=x,yy=y)
        if(nrow(df[!is.na(df$yy),])>8){
          df$yy <- as.numeric(forecast::na.interp(df$yy))
          return(df$yy)}else{
          return(df$yy)}}
      
      # IMPUTE STACK 
      
      my_brns_tile_impute <- st_apply(my_brns_tile,
                                      MARGIN = c("x", "y"),
                                      FUN = my_func_impute,
                                      x = st_get_dimension_values(my_brns_tile, "band"))
      
      # COPY DIMENSIONS
      
      my_brns_tile_impute <- st_set_dimensions(my_brns_tile_impute,
                                               which = 1,
                                               values = st_get_dimension_values(my_brns_tile, "band"),
                                               names = "band")
      
      # WRITE 
      
      write_stars(my_brns_tile_impute, paste0(out_master_folder_full,"/",dirrr,"/brns/",impute_name))
      write_stars(my_brns_tile_impute>(brns_th/10), paste0(out_master_folder_full,"/",dirrr,"/brns/",sub(".tif",paste0("_",brns_th,".tif"),impute_name)), type = "Byte")
      # }

  # Export smooth raster ####
    
    loess_span = "08"
    smooth_name <- paste0(p,"_brns_loess_",loess_span,".tif")

    # if(smooth_name %in% brns_files){
    # 
    #   # READ IF EXISTS
    # 
    #   my_brns_tile_smooth <- read_stars(paste0(out_master_folder_full,"/",dirrr,"/brns/",smooth_name))
    # 
    # }else{

      # SMOOTH FUNCTION

      my_func_smooth <- function(y,x) {
        df <- data.frame(xx=x,yy=y)
        if(nrow(df[!is.na(df$yy),])>8){
          return(stats::predict(stats::loess(yy~xx, df, span = as.numeric(loess_span)/10), df$xx)
          )}else{return(df$yy)}}
      

      # IMPUTE STACK

      my_brns_tile_smooth <- st_apply(my_brns_tile_impute, MARGIN = c("x", "y"),
                                      FUN = my_func_smooth, x = st_get_dimension_values(my_brns_tile_impute, "band"))
      
      # COPY DIMENSIONS

      my_brns_tile_smooth <- st_set_dimensions(my_brns_tile_smooth, which = 1,
                                               values = st_get_dimension_values(my_brns_tile_impute, "band"), names = "band")
      
      # WRITE

      write_stars(my_brns_tile_smooth, paste0(out_master_folder_full,"/",dirrr,"/brns/",smooth_name))
      write_stars(my_brns_tile_smooth>(brns_th/10), paste0(out_master_folder_full,"/",dirrr,"/brns/",sub(".tif",paste0("_",brns_th,".tif"),smooth_name)), type = "Byte")
      # }
    
  # Export moving average ####
    
    order <-  2
    moving_name <- paste0(p,"_brns_moving_",order,".tif")
    
    # if(moving_name %in% brns_files){
    # 
    #   # READ IF EXISTS
    # 
    #   my_brns_tile_moving <- read_stars(paste0(out_master_folder_full,"/",dirrr,"/brns/",moving_name))
    # 
    # }else{
      
      # SMOOTH FUNCTION
      
      my_func_ma <- function(y,x) {
        df <- data.frame(xx=x,yy=y)
        if(nrow(df[!is.na(df$yy),])>8){
          return(as.numeric(caTools::runmean(df$yy, k = order))
          )}else{return(df$yy)}}
      
      
      # IMPUTE STACK
      
      my_brns_tile_ma <- st_apply(my_brns_tile_impute, MARGIN = c("x", "y"),
                                      FUN = my_func_ma, x = st_get_dimension_values(my_brns_tile_impute, "band"))
      
      # COPY DIMENSIONS
      
      my_brns_tile_ma <- st_set_dimensions(my_brns_tile_ma, which = 1,
                                           values = st_get_dimension_values(my_brns_tile_impute, "band"), names = "band")
      
      # WRITE
      
      write_stars(my_brns_tile_ma, paste0(out_master_folder_full,"/",dirrr,"/brns/",moving_name))
      
      write_stars(my_brns_tile_ma>(brns_th/10), paste0(out_master_folder_full,"/",dirrr,"/brns/",sub(".tif",paste0("_",brns_th,".tif"),moving_name)), type = "Byte")
    # }        

#### NDWI ####
      
  ndwi_th <-  45              
      
  # List files for tile ####
      
    ndwi_files <- list.files(
      path = paste0(out_master_folder_full,"/",dirrr,"/ndwi/"), 
      pattern = paste0("^",p,"_"))
    
  # Export raw raster ####
      
    raw_name <- paste0(p,"_ndwi.tif")
      
  #   if(raw_name %in% ndwi_files){
  # 
  # # READ IF EXISTS
  # 
  #   my_ndwi_tile <- read_stars(paste0(out_master_folder_full,"/",dirrr,"/ndwi/",raw_name))
  # 
  #   }else{
        
        # CLIP 
        
        my_ndwi_tile <- my_ndwi[polys[p,]] %>% st_as_stars()
        
        # REMOVE NA 
        
        my_ndwi_tile[my_ndwi_tile==-9] = NA
        # my_ndwi_tile[my_ndwi_tile<0] = NA
        
        # WRITE 
        
        write_stars(my_ndwi_tile, paste0(out_master_folder_full,"/",dirrr,"/ndwi/",raw_name))
        
      # }
      
      # Export imputed raster ####
      
      impute_name <- paste0(p,"_ndwi_impute.tif")
      
      # if(impute_name %in% ndwi_files){
      # 
      #   # READ IF EXISTS
      # 
      #   my_ndwi_tile_impute <- read_stars(paste0(out_master_folder_full,"/",dirrr,"/ndwi/",impute_name))
      # 
      # }else{
        
        # IMPUTE FUNCTION 
        
        my_func_impute <- function(y,x) {
          df <- data.frame(xx=x,yy=y)
          if(nrow(df[!is.na(df$yy),])>8){
            df$yy <- as.numeric(forecast::na.interp(df$yy))
            return(df$yy)}else{
              return(df$yy)}}
        
        # IMPUTE STACK 
        
        my_ndwi_tile_impute <- st_apply(my_ndwi_tile,
                                        MARGIN = c("x", "y"),
                                        FUN = my_func_impute,
                                        x = st_get_dimension_values(my_ndwi_tile, "band"))
        
        # COPY DIMENSIONS
        
        my_ndwi_tile_impute <- st_set_dimensions(my_ndwi_tile_impute,
                                                 which = 1,
                                                 values = st_get_dimension_values(my_ndwi_tile, "band"),
                                                 names = "band")
        
        # WRITE 
        write_stars(my_ndwi_tile_impute, paste0(out_master_folder_full,"/",dirrr,"/ndwi/",impute_name))
        
        write_stars(my_ndwi_tile_impute>(ndwi_th/100), paste0(out_master_folder_full,"/",dirrr,"/ndwi/",sub(".tif",paste0("_",ndwi_th,".tif"),impute_name)), type = "Byte")
      # }
    
      # Export smooth raster ####
      
      loess_span = "03"
      smooth_name <- paste0(p,"_ndwi_loess_",loess_span,".tif")
      
      # if(smooth_name %in% ndwi_files){
      # 
      #   # READ IF EXISTS
      # 
      #   my_ndwi_tile_smooth <- read_stars(paste0(out_master_folder_full,"/",dirrr,"/ndwi/",smooth_name))
      # 
      # }else{
        
        # SMOOTH FUNCTION
        
        my_func_smooth <- function(y,x) {
          df <- data.frame(xx=x,yy=y)
          if(nrow(df[!is.na(df$yy),])>8){
            return(stats::predict(stats::loess(yy~xx, df, span = as.numeric(loess_span)/10), df$xx)
            )}else{return(df$yy)}}
        
        
        # IMPUTE STACK
        
        my_ndwi_tile_smooth <- st_apply(my_ndwi_tile_impute, MARGIN = c("x", "y"),
                                        FUN = my_func_smooth, x = st_get_dimension_values(my_ndwi_tile_impute, "band"))
        
        # COPY DIMENSIONS
        
        my_ndwi_tile_smooth <- st_set_dimensions(my_ndwi_tile_smooth, which = 1,
                                                 values = st_get_dimension_values(my_ndwi_tile_impute, "band"), names = "band")
        
        # WRITE
        
      write_stars(my_ndwi_tile_smooth, paste0(out_master_folder_full,"/",dirrr,"/ndwi/",smooth_name))
      write_stars(my_ndwi_tile_smooth>(ndwi_th/100), paste0(out_master_folder_full,"/",dirrr,"/ndwi/",sub(".tif",paste0("_",ndwi_th,".tif"),smooth_name)), type = "Byte")
      # }
      
      # Export moving average ####
      
      order <-  2
      moving_name <- paste0(p,"_ndwi_moving_",order,".tif")
      
      # if(moving_name %in% ndwi_files){
      # 
      #   # READ IF EXISTS
      # 
      #   my_ndwi_tile_moving <- read_stars(paste0(out_master_folder_full,"/",dirrr,"/ndwi/",moving_name))
      # 
      # }else{
        
        # SMOOTH FUNCTION
        
        my_func_ma <- function(y,x) {
          df <- data.frame(xx=x,yy=y)
          if(nrow(df[!is.na(df$yy),])>8){
            return(caTools::runmax(df$yy, k = 4)
            )}else{return(df$yy)}}
        
        
        # IMPUTE STACK
        
        my_ndwi_tile_ma <- st_apply(my_ndwi_tile_impute, MARGIN = c("x", "y"),
                                    FUN = my_func_ma, x = st_get_dimension_values(my_ndwi_tile_impute, "band"))
        
        # COPY DIMENSIONS
        
        my_ndwi_tile_ma <- st_set_dimensions(my_ndwi_tile_ma, which = 1,
                                             values = st_get_dimension_values(my_ndwi_tile_impute, "band"), names = "band")
        # plot(my_ndwi_tile_ma>0.45)
        # WRITE
        
        write_stars(my_ndwi_tile_ma, paste0(out_master_folder_full,"/",dirrr,"/ndwi/",moving_name))
        write_stars(my_ndwi_tile_ma>(ndwi_th/100), paste0(out_master_folder_full,"/",dirrr,"/ndwi/",sub(".tif",paste0("_",ndwi_th,".tif"),moving_name)), type = "Byte")
      # }
      file.remove(paste0(out_master_folder_full,"/",dirrr,"/",p,".txt"))
      }
stopCluster(cl)  



# lapply(list.files(
#   path = paste0(out_master_folder_full,"/",dirrr,"/"), recursive = T,  
#   pattern = "txt", full.names = T), file.remove)
