library(sf)
library(dplyr)

# RUN ALL

  years <- 1984:2021

#### 0. Parameters ####
  
  gdrive <- "E:/Google Drive/"
  folder <- "GlacierChangeBC_Paper_rgee_2021"
  out_master_folder_full <- paste0(gdrive,folder,"/")

  indices <- c("brns","ndwi")
  
  folder_raw <-  "0_gee_out_tif_tile_raw/"
  dir.create(paste0(out_master_folder_full,folder_raw), showWarnings = F)
  
  folder_smooth <-  "1_gee_out_tif_tile_smooth/"
  dir.create(paste0(out_master_folder_full,folder_smooth), showWarnings = F)
  
  folder_poly <- "2_smooth_polys/"
  dir.create(paste0(out_master_folder_full,folder_poly), showWarnings = F)

  lapply(indices, function(index){dir.create(paste0(out_master_folder_full,folder_raw,index), showWarnings = F)})
  lapply(indices, function(index){dir.create(paste0(out_master_folder_full,folder_smooth,index), showWarnings = F)})
    
#### 0. Move files to subdirs #### 
  
  # files <- list.files(out_master_folder_full, pattern = "_brns_", full.names = F)
  # lapply(files, function(f){
  #   from <- paste0(out_master_folder_full,f)
  #   to <- paste0(out_master_folder_full,folder_raw,"brns/",f)
  #   file.copy(from, to)  
  #   file.remove(from)
  #   })
  # 
  # files <- list.files(out_master_folder_full, pattern = "_ndwi_", full.names = F)
  # lapply(files, function(f){
  #   from <- paste0(out_master_folder_full,f)
  #   to <- paste0(out_master_folder_full,folder_raw,"ndwi/",f)
  #   file.copy(from, to)  
  #   file.remove(from)
  #   })
  
#### 1. VRT OF ANNUAL MOSAIC TILES #### 
  
  #MAKE SURE TO MOVE THE RAW DATA TO THE FOLDER_RAW/INDEX FOLDER
  # source("Code/2_local/1_vrt.R")
  
#### 2. MAKE TILES VECTOR ####
  
  my_brns <- stars::read_stars(paste0(out_master_folder_full,folder_raw,"/brns_3005.vrt"))
  
  glaciers1985 <- sf::read_sf("Data/glaciers14411.sqlite") %>% 
    mutate(n = row_number(), area_new = st_area(.)) %>% 
    st_transform(3005) %>% 
    st_make_valid()
  
  tile_size <- 10
  # tiles_sf <- sf::st_make_grid(sf::st_as_sfc(sf::st_bbox(my_brns)), cellsize = c(tile_size*1000,tile_size*1000), crs = 3005, what = 'polygons')
  # tiles_sf <- tiles_sf[glaciers1985, , op = st_intersects] %>% sf::st_as_sf()
  # write_sf(tiles_sf, paste0("Data/tiles_",tile_size, "km_intersect.gpkg"))
  tiles_sf <- sf::read_sf(paste0("Data/tiles_",tile_size, "km_intersect.gpkg"))
  
#### 3. SMOOTH STACK PER TILE (~4.5 hours for 36 bands) ####
  
  brns_th <- 1.8
  ndwi_th <- 0.45
  # source("Code/2_local/2_vrt_to_smooth.R")
  
#### 4. VRT OF SMOOTH TILES ####

  # source("Code/2_local/3_smooth_tile_to_vrt.R")

#### CLIP TO GLACIERS #### 
  
  # folder_glTif <- "4_gl_tif/"
  # dir.create(paste0(out_master_folder_full,folder_glTif), showWarnings = F)
  # source("Code/2_local/4_smooth_tif_to_tif_perglaciers.R")
  
#### 5. VRT TO POLY ####

  brns_fill_th <- 0.01
  brns_area_th <- 0.05
  ndwi_fill_th <- 0.01
  ndwi_area_th <- 0.01

  source("Code/2_local/4_smooth_vrt_to_poly.R")
