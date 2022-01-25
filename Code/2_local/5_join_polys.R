

indices <- c("moving","loess","impute")

# plan(multisession, workers = parallel::detectCores())

lapply(indices, function(index){
  
  l <- list.files(paste0("E:/Google Drive/",folder,"/", folder_poly), paste0(index, "_18_4.5.gpkg"), full.names = T)

  for(i in l){
    
    print(i)
    
    sf::gdal_utils(util = "vectortranslate", 
                   source = i, 
                   destination = paste0("E:/Google Drive/",folder,"/", index, "_tonight.gpkg"),
                   options = c(
                     "-f", "GPKG", # output file format for GDAL < 2.3
                     "-s_srs", "EPSG:3005", # input file SRS
                     "-t_srs", "EPSG:3005", # output file SRS
                     "-nln","glacier",
                     "-nlt","MULTIPOLYGON",
                     "-overwrite", 
                     "-append",
                     "-update"))
    
      }

  l <- list.files(paste0("E:/Google Drive/",folder,"/", folder_poly), paste0(index, "_18_4.5_ndwi.gpkg"), full.names = T)
  
  for(i in l){
    
    print(i)
    
    sf::gdal_utils(util = "vectortranslate", 
                   source = i, 
                   destination = paste0("E:/Google Drive/",folder,"/", index, "_tonight_ndwi.gpkg"),
                   options = c(
                     "-f", "GPKG", # output file format for GDAL < 2.3
                     "-s_srs", "EPSG:3005", # input file SRS
                     "-t_srs", "EPSG:3005", # output file SRS
                     "-nln","glacier",
                     "-nlt","MULTIPOLYGON",
                     "-overwrite", 
                     "-append",
                     "-update"))
    
  }
  
    return(index)
  })

# future:::ClusterRegistry("stop")
