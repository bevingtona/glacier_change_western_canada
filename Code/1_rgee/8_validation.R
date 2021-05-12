library(tidyverse)
library(sf)
library(rgee)
# ee_reattach() # reattach ee as a reserved word

valpol <- read_sf("2_Paper_BCA_Glacier/0_Data/Validation_Glaciers/my_val_glaciers_poly.shp") %>% st_transform(4326)


ee_Initialize(email = "bevingtona@gmail.com", drive = T)

for(my_valpol_id in valpol$Id){
  
  rgee::ee_reattach()
  # my_valpol_id = valpol[1,]$Id
  print(my_valpol_id)
  
  bbox <- valpol %>% filter(Id == my_valpol_id) %>% st_bbox()
  aoi <- ee$Geometry$Rectangle(c(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]]))
  
  source("2_Paper_BCA_Glacier/2_R_Code/1_rgee/1_imports.R")
  source("2_Paper_BCA_Glacier/2_R_Code/1_rgee/2_parameters.R")
  source("E:/Dropbox/FLNRO_p1/Research_Cryosphere/Project_Glacier/2019_Bevinton_Thesis/R_Thesis/2_Paper_BCA_Glacier/2_R_Code/1_rgee/f_rasters.R")
  
  L4SR1 = ee$ImageCollection('LANDSAT/LT04/C01/T1_SR');
  L5SR1 = ee$ImageCollection('LANDSAT/LT05/C01/T1_SR');
  L7SR1 = ee$ImageCollection('LANDSAT/LE07/C01/T1_SR')$filterDate('1999-01-01','2003-01-01');
  L8SR1 = ee$ImageCollection('LANDSAT/LC08/C01/T1_SR');
  
  LT_BANDS = c('B1', 'B2', 'B3', 'B4', 'B5', 'B7', 'B6');
  LE_BANDS = c('B1', 'B2', 'B3', 'B4', 'B5', 'B7', 'B6');
  LC_BANDS = c('B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B10');
  ST_NAMES = c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'tir');
  
  collection = L4SR1$select(LT_BANDS, ST_NAMES)$merge(
    L5SR1$select(LT_BANDS, ST_NAMES))$merge(
    L7SR1$select(LE_BANDS, ST_NAMES))$merge(
    L8SR1$select(LC_BANDS, ST_NAMES))$
    # filterDate("1984-01-01", "1990-05-01")$
    filter(ee$Filter$calendarRange(7,8,"month"))$
    filterMetadata("CLOUD_COVER","less_than",10)$
    filterBounds(aoi)$
    map(srScale)$
    map(radiometric)$
    # map(cloudMask)$
    map(computeGlaciersAndSnow)$
    map(myclip_aoi);
  
  
  size <- collection$size()$getInfo()
  print(size)
  # print(collection$first()$getInfo())
  
  x <- lapply(0:size-1, function(i){
    
  image <- ee$Image(collection$toList(collection$size())$get(i))$clip(aoi)
  # print(image$bandNames()$getInfo())
  # Map$addLayer(image, list(bands = c("swir2", "nir", "red"), max = 0.3), "median")
  name <- image$get("LANDSAT_ID")$getInfo()
  
  downConfig <- list(
    scale = imageRes,
    maxPixels = 1.0E13,
    # crs = "EPSG:3005",
    driveFolder = "GlacierChangeBC_Paper_rgee_validation",
    region = aoi)
  
  task <- ee$batch$Export$image(image,
  paste0(my_valpol_id,"_",name,"_nocrs"),
  downConfig)
  task$start()
  })
}


# all_task <- ee$data$getTaskList()
# running_task <- which(unlist(lapply(all_task, "[[", "state")) == "READY")
# running <- all_task[running_task]
# 
# for(z in seq_along(running)){
#   print(paste(z, round(z/length(running_task))))
#   ee$data$cancelTask(running[[z]][["id"]])}
  
  
