
library(future.apply)
  
  dat <- c("raw","impute","loess","moving") 
  
  for(index in indices){
    print(index)
    
    plan(multisession, workers = 4)
    future_lapply(dat, function(da){
      
        mysearch <- paste0(index,"_",da,"_th.tif")
        list <- list.files(path = paste0(out_master_folder_full,folder_smooth,index), pattern = paste0(mysearch,"$"), recursive = T, full.names = T)
        sf::gdal_utils(util = "buildvrt", 
                       source = list, 
                       destination = paste0(out_master_folder_full,folder_smooth, sub("tif","vrt",mysearch)),
                       options = c("seperate","0",
                                   "-srcnodata", "0",
                                   "-r", "nearest",
                                   "-overwrite"))
    return(mysearch)
        })
    
    future:::ClusterRegistry("stop")
    }
  