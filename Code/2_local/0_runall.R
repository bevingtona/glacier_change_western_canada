
# RUN ALL

#### 0. Parameters ####

  # Master Folder
  out_master_folder_full <- paste0("E:/Google Drive/",folder,"/")
  # out_master_folder <- sub(".tif","",str_split_fixed(string = list.files(gDrive)[1], pattern = "_", n = 4)[,4])
  # out_master_folder_full <- paste0(out_master_location, out_master_folder, "/")
  # dir.create(out_master_folder_full)

#### 1. VRT OF ANNUAL MOSAIC TILES #### 
  
  infolder <-  "0_gee_out_tif_tile_raw/"
  dir.create(paste0(out_master_folder_full,infolder))
  
  source("Code/2_local/1_vrt.R")
  

  gdalUtils::gdalwarp(s_srs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                      t_srs = "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
                      of="VRT",
                      srcfile = paste0(out_master_folder_full,infolder,"ndwi.vrt"),
                      dstfile = paste0(out_master_folder_full,infolder,"ndwi3005.vrt"))
  
  #### 2. MAKE TILES VECTOR ####
  
  my_brns <- stars::read_stars(paste0(out_master_folder_full,"/",infolder,"/brns3005.vrt"))
  glaciers1985 <- sf::read_sf("../../2018_RGI2/manuscript_outlines/Bolch/glacinvent_1985/bolch_1985_clean_dissolve_pol.shp")
  
  tiles <- GSIF::getSpatialTiles(as_Spatial(st_as_sf(st_as_sfc(st_bbox(my_brns)))), block.x=5000, block.y=5000, overlap.percent = 1)
  tiles_sf <- st_as_sf(tiles)
  write_sf(tiles_sf, paste0(out_master_folder_full,"/",infolder,"/tiles_5x5km.gpkg"))
  
  tile_sf <- tiles_sf[glaciers1985, , op = st_intersects]
  write_sf(tile_sf, paste0(out_master_folder_full,"/",infolder,"/tiles_5x5km_intersect.gpkg"))
  
  #### 3. SMOOTH STACK PER TILE (~4.5 hours for 36 bands) ####
  
  # 6 hours with 48 cores 
  
  polys <- sf::read_sf(paste0(out_master_folder_full,"/",infolder,"/tiles_5x5km_intersect.gpkg"))
  dirrr <- "1_gee_out_tif_tile_smooth_NAfill"
  
  source("2_Paper_BCA_Glacier/2_R_Code/2_local/3_vrt_to_smooth.R")
  
  #### 4. VRT OF SMOOTH TILES ####
  
  # BRNS 
  
  indd <- c("brns")
  suffs <- c("_impute_18","_loess_08_18","_moving_2_18")
  
  for(ind in indd){
    print(ind)
    for(suf in suffs){
      print(suf)
      
      files <- list.files(path = paste0(out_master_folder_full,"/",dirrr,"/",ind,"/"), 
                          pattern = paste0(suf,".tif$"), recursive = T, full.names = T)
      index <- c(seq(1,length(files),100),length(files))
      
      lapply(1:(length(index)-1), function(i){
        gdalUtils::gdalbuildvrt(gdalfile = files[index[i]:index[i+1]], 
                                output.vrt = paste0(out_master_folder_full,"/",dirrr,"/",ind,"/",ind,"_",index[i],suf,".vrt"), 
                                separate = 0,
                                overwrite = T,
                                a_srs = "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
                                r = "bilinear", 
                                srcnodata = 0)})
      
      files <- list.files(path = paste0(out_master_folder_full,"/",dirrr,"/",ind,"/"), 
                          pattern = paste0(ind,".*",suf,".vrt$"), recursive = T, full.names = T)
      gdalUtils::gdalbuildvrt(gdalfile = files, 
                              output.vrt = paste0(out_master_folder_full,"/",dirrr,"/",ind,suf,".vrt"), 
                              separate = 0,
                              a_srs = "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
                              overwrite = T,
                              r = "bilinear")}}
  
  # NDWI
  
  indd <- c("ndwi")
  suffs <- c("_impute_45","_loess_03_45","_moving_2_45")
  
  for(ind in indd){
    print(ind)
    for(suf in suffs){
      print(suf)
      
      files <- list.files(path = paste0(out_master_folder_full,"/",dirrr,"/",ind,"/"), 
                          pattern = paste0(suf,".tif$"), recursive = T, full.names = T)
      index <- c(seq(1,length(files),100),length(files))
      
      lapply(1:(length(index)-1), function(i){
        gdalUtils::gdalbuildvrt(gdalfile = files[index[i]:index[i+1]], 
                                output.vrt = paste0(out_master_folder_full,"/",dirrr,"/",ind,"/",ind,"_",index[i],suf,".vrt"), 
                                separate = 0,
                                overwrite = T,
                                a_srs = "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
                                r = "bilinear", 
                                srcnodata = 0)})
      
      # files <- list.files(path = paste0(out_master_folder_full,"/",dirrr,"/",ind,"/"), 
     
  brns_name <- "brns_impute_18"
  ndwi_name <- "ndwi_impute_45"
  cores <- parallel::detectCores()-5
  source("2_Paper_BCA_Glacier/2_R_Code/2_local/5_smooth_vrt_to_poly.R")
  brns_name <- "brns_loess_08_18"
  ndwi_name <- "ndwi_loess_03_45"
  source("2_Paper_BCA_Glacier/2_R_Code/2_local/5_smooth_vrt_to_poly.R")
  brns_name <- "brns_moving_2_18"
  ndwi_name <- "ndwi_moving_2_45"
  source("2_Paper_BCA_Glacier/2_R_Code/2_local/5_smooth_vrt_to_poly.R")                     
  
  # pattern = paste0(ind,".*",suf,".vrt$"), recursive = T, full.names = T)
      gdalUtils::gdalbuildvrt(gdalfile = files, 
                              output.vrt = paste0(out_master_folder_full,"/",dirrr,"/",ind,suf,".vrt"), 
                              separate = 0,
                              a_srs = "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
                              overwrite = T,
                              r = "bilinear")}}
  
  #### 5. VRT TO POLY ####
  
  
  # #### 6. MERGE GLACIERS AND YEARS ####
  
  brns_name <- "brns_impute_18"
  ndwi_name <- "ndwi_impute_45"
  cores <- parallel::detectCores()
  
  brns_name <- "brns_loess_08_18"
  ndwi_name <- "ndwi_loess_03_45"
  source("2_Paper_BCA_Glacier/2_R_Code/2_local/6_sqlite_tomaster_gpkg_pYear.R")
  
  brns_name <- "brns_moving_2_18"
  ndwi_name <- "ndwi_moving_2_45"
  source("2_Paper_BCA_Glacier/2_R_Code/2_local/6_sqlite_tomaster_gpkg_pYear.R")
  
  stopCluster(cl)
  # 
  # source("2_Paper_BCA_Glacier/2_R_Code/2_local/4_smooth_to_poly_clean.R")
