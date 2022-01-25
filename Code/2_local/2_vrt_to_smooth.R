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
library(future.apply)

# FOREACH TILE SMOOTH
plan(multisession, workers = detectCores()-4)

future_lapply(1:nrow(tiles_sf), function(tile_no){
  
  # Pick tile
    tile_sf <- tiles_sf[tile_no,]
  
  # Loop indices
    perIndex <- lapply(indices, function(index){
  
      # write.csv("", paste0(out_master_folder_full,folder_smooth,tile_no, ".txt"))
      
    # Check if files exist
      checklist <- list.files(path = paste0(out_master_folder_full,folder_smooth,index,"/"), pattern = paste0("^",tile_no,"_",index))
      
      if(str_detect(checklist, "moving_th.tif") %>% sum() == 1){
        print("done")
      } else { 
        

        write.csv("", paste0(out_master_folder_full,folder_smooth,tile_no,index,"RUN.txt"))
        
        # PARAMETERS 
        
        if(index == "brns"){
          loess_span <- "08"
          th <- brns_th
          moving_order <- 2
          }
        
        if(index == "ndwi"){
          loess_span <- "03"
          th <- ndwi_th
          moving_order <- 2
        }
        
        # LOAD DATA 
        
          raw_stack <- stars::read_stars(paste0(out_master_folder_full,folder_raw,index,"_3005.vrt"), proxy = T)
          raw_stack_tile <- raw_stack[tile_sf] %>% st_as_stars()
          raw_stack_tile[raw_stack_tile==-9] = NA
          raw_stack_tile <- raw_stack_tile[glaciers1985 %>% st_intersection(tile_sf)]
          
        # IMPUTE STACK 
        
          if(str_detect(checklist, "impute.tif") %>% sum() == 1){
            raw_stack_tile_impute <- read_stars(paste0(out_master_folder_full,folder_smooth,index,"/",tile_no,"_",index,"_impute.tif"))
          } else { 
            raw_stack_tile_impute <- st_apply(raw_stack_tile,
                                          MARGIN = c("x", "y"),
                                          FUN = function(y,x) {
                                            df <- data.frame(xx=x,yy=y)
                                            if(nrow(df[!is.na(df$yy),])>8){
                                              df$yy <- as.numeric(forecast::na.interp(df$yy))
                                              return(df$yy)}else{
                                                return(df$yy)}},
                                          x = st_get_dimension_values(raw_stack_tile, "band"))
          
          raw_stack_tile_impute <- st_set_dimensions(raw_stack_tile_impute,
                                                   which = 1,
                                                   values = st_get_dimension_values(raw_stack_tile, "band"),
                                                   names = "band")}
        
        # SMOOTH LOESS
          
          if(str_detect(checklist, "loess.tif") %>% sum() == 1){
            raw_stack_tile_impute <- read_stars(paste0(out_master_folder_full,folder_smooth,index,"/",tile_no,"_",index,"_loess.tif"))
          } else {
            raw_stack_tile_loess <- st_apply(raw_stack_tile_impute,
                                            MARGIN = c("x", "y"),
                                            FUN = function(y,x) {
                                              df <- data.frame(xx=x,yy=y)
                                              if(nrow(df[!is.na(df$yy),])>8){
                                                return(stats::predict(stats::loess(yy~xx, df, span = as.numeric(loess_span)/10), df$xx)
                                                )}else{return(df$yy)}},
                                            x = st_get_dimension_values(raw_stack_tile_impute, "band"))
          
          raw_stack_tile_loess <- st_set_dimensions(raw_stack_tile_loess,
                                                     which = 1,
                                                     values = st_get_dimension_values(raw_stack_tile, "band"),
                                                     names = "band")}
          
        # MOVING MEAN
          
          if(str_detect(checklist, "moving.tif") %>% sum() == 1){
            raw_stack_tile_impute <- read_stars(paste0(out_master_folder_full,folder_smooth,index,"/",tile_no,"_",index,"_moving.tif"))
          } else {
            raw_stack_tile_moving <- st_apply(raw_stack_tile_impute,
                                           MARGIN = c("x", "y"),
                                           FUN = function(y,x) {
                                             df <- data.frame(xx=x,yy=y)
                                             if(nrow(df[!is.na(df$yy),])>8){
                                               return(as.numeric(caTools::runmean(df$yy, k = moving_order))
                                                      )}else{return(df$yy)}},
                                           x = st_get_dimension_values(raw_stack_tile_impute, "band"))
          
          raw_stack_tile_moving <- st_set_dimensions(raw_stack_tile_moving,
                                                    which = 1,
                                                    values = st_get_dimension_values(raw_stack_tile, "band"),
                                                    names = "band")}
          
        # EXPORT 
          
          # write_stars(raw_stack_tile, paste0(out_master_folder_full,folder_smooth,index,"/",tile_no,"_",index,"_raw.tif"))
          raw_stack_tile_th <- raw_stack_tile>th
          write_stars(raw_stack_tile_th, paste0(out_master_folder_full,folder_smooth,"/",index,"/",tile_no,"_",index,"_raw_th.tif"), type = "Byte", NA_value = 0)
          
          # write_stars(raw_stack_tile_impute, paste0(out_master_folder_full,folder_smooth,index,"/",tile_no,"_",index,"_impute.tif"))
          raw_stack_tile_impute_th <- raw_stack_tile_impute>th
          write_stars(raw_stack_tile_impute_th, paste0(out_master_folder_full,folder_smooth,"/",index,"/",tile_no,"_",index,"_impute_th.tif"), type = "Byte", NA_value = 0)
          
          # write_stars(raw_stack_tile_loess, paste0(out_master_folder_full,folder_smooth,index,"/",tile_no,"_",index,"_loess.tif"))
          raw_stack_tile_loess_th <- raw_stack_tile_loess>th
          write_stars(raw_stack_tile_loess_th, paste0(out_master_folder_full,folder_smooth,"/",index,"/",tile_no,"_",index,"_loess_th.tif"), type = "Byte", NA_value = 0)
          
          # write_stars(raw_stack_tile_moving, paste0(out_master_folder_full,folder_smooth,index,"/",tile_no,"_",index,"_moving.tif"))
          raw_stack_tile_moving_th <- raw_stack_tile_moving>th
          write_stars(raw_stack_tile_moving_th, paste0(out_master_folder_full,folder_smooth,"/",index,"/",tile_no,"_",index,"_moving_th.tif"), type = "Byte", NA_value = 0)
        
          
      file.remove(paste0(out_master_folder_full,folder_smooth,tile_no,index,"RUN.txt"))
      
      return(index)
      }
      }
      # }
    ) 
      
    
    tile_no
    }
  )

future:::ClusterRegistry("stop")
