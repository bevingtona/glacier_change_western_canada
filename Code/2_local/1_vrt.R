library(gdalUtils)
    
indices <- c("brns","ndwi")

# for(index in indices){
  band=indices[1]
  files <- list.files(path = paste0(out_master_folder_full,infolder,band,"/"), 
                      recursive = T, full.names = T, pattern = "*.tif$")
  index <- c(seq(1,length(files),100),length(files))
  
  lapply(1:(length(index)-1), function(i){
    sf::gdal_utils(util = "buildvrt", 
                   source = files[index[i]:index[i+1]], 
                   destination = paste0(out_master_folder_full,"/",infolder,band,"/",band,"_",index[i],".vrt"), 
                   options = c("seperate","0", 
                               "-overwrite", "T",
                               "-r", "bilinear",
                               "-srcnodata", "0"))})
    
  files <- list.files(path = paste0(out_master_folder_full,"/",infolder,"/",band,"/"), 
                      pattern = paste0(band,".*.vrt$"), recursive = T, full.names = T)
  gdalUtils::gdalbuildvrt(gdalfile = files, 
                          output.vrt = paste0(out_master_folder_full,"/",infolder,"/brns.vrt"), 
                          separate = 0,
                          overwrite = T,
                          r = "bilinear")
  gdalUtils::gdalwarp(s_srs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                      t_srs = "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
                      of="VRT",
                      srcfile = paste0(out_master_folder_full,infolder,"brns.vrt"),
                      dstfile = paste0(out_master_folder_full,infolder,"brns3005.vrt"))
# NDWI
  
  files <- list.files(path = paste0(out_master_folder_full,infolder,"ndwi/"), 
                      pattern = "*.tif$", recursive = T, full.names = T)
  index <- c(seq(1,length(files),100),length(files))
  
  lapply(1:(length(index)-1), function(i){
    gdalUtils::gdalbuildvrt(gdalfile = files[index[i]:index[i+1]], 
                            a_srs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                            output.vrt = paste0(out_master_folder_full,"/",infolder,"/ndwi/ndwi_",index[i],".vrt"), 
                            separate = 0,
                            overwrite = T,
                            r = "bilinear", 
                            srcnodata = 0)})
  
  files <- list.files(path = paste0(out_master_folder_full,"/",infolder,"/ndwi/"), 
                      pattern = "ndwi.*.vrt$", recursive = T, full.names = T)
  gdalUtils::gdalbuildvrt(gdalfile = files, 
                          output.vrt = paste0(out_master_folder_full,"/",infolder,"/ndwi.vrt"), 
                          separate = 0,
                          overwrite = T,
                          r = "bilinear")
