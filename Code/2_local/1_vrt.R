# band = indices[1]
# i=1

lapply(indices, function(band){
  
  print(band)
  
  files <- list.files(path = paste0(out_master_folder_full,folder_raw,band,"/"), 
                      recursive = T, full.names = T, pattern = "*.tif$")
  
  index <- c(seq(1,length(files),100),length(files))
  
  lapply(1:(length(index)-1), function(i){
    sf::gdal_utils(util = "buildvrt", 
                   source = files[index[i]:index[i+1]], 
                   destination = paste0(out_master_folder_full,folder_raw,band,"/",band,"_",index[i],".vrt"), 
                   options = c("seperate","0", 
                               "-overwrite", "T",
                               "-r", "bilinear",
                               "-srcnodata", "0"))})
    
  files <- list.files(path = paste0(out_master_folder_full,folder_raw,band), 
                      pattern = paste0(band,".*.vrt$"), recursive = T, full.names = T)
  
  sf::gdal_utils(util = "buildvrt", 
                 source = files, 
                 destination = paste0(out_master_folder_full,folder_raw,band,".vrt"), 
                 options = c("seperate","0", 
                             "-overwrite", "T",
                             "-r", "bilinear",
                             "-srcnodata", "0"))
  
  sf::gdal_utils(util = "warp", 
                 source = paste0(out_master_folder_full,folder_raw,band,".vrt"), 
                 destination = paste0(out_master_folder_full,folder_raw,band,"_3005.vrt"), 
                 options = c("-of", "VRT", # output file format for GDAL < 2.3
                             "-s_srs", "EPSG:4326", # input file SRS
                             "-t_srs", "EPSG:3005", # output file SRS
                             "-overwrite"
                             ))
  
  })
  