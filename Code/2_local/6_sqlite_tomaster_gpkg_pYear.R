library(tidyverse)
library(smoothr)
library(raster)
library(stars)
library(sf)
library(tidyverse)
library(doParallel)
library(foreach)   

fol <- "2_smooth_polys_NAfill"
outfolder <- paste0(out_master_folder_full,"/",fol,"/",brns_name)


cl <- parallel::makeCluster(cores)
doParallel::registerDoParallel(cl)

x = foreach(y = 1984:2019,
            .combine = 'rbind',
            .packages = c("sf","dplyr"),
            .export = c("outfolder","brns_name","ndwi_name","out_master_folder_full","fol")) %dopar% 
  {
  
  my_read_write_sf_g <- function(index){
    # GLACIER
    my_sf_g <- read_sf(paste0(outfolder,"/",index,"/",index,"_",y,"_glacier_",brns_name,"_",ndwi_name,".sqlite")) %>% st_cast("MULTIPOLYGON")
    st_write(my_sf_g, 
             paste0("F:/",y,"_glacier_",brns_name,"_",ndwi_name,".gpkg"), 
             layer = paste0(y,"_glacier_",brns_name,"_",ndwi_name), append=TRUE, quiet = T)
    # WATER
    my_sf_w <- read_sf(paste0(outfolder,"/",index,"/",index,"_",y,"_water_",brns_name,"_",ndwi_name,".sqlite")) %>% st_cast("MULTIPOLYGON")
    st_write(my_sf_w, 
             paste0("F:/",y,"_water_",brns_name,"_",ndwi_name,".gpkg"), 
             layer = paste0(y,"_water_",brns_name,"_",ndwi_name), append=TRUE, quiet = T)
    }
  lapply(1:14411, my_read_write_sf_g)
  
  }
  
stopCluster(cl)
