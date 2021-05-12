library(fasterize)
library(sf)
library(raster)
library(doParallel)
library(foreach) 

# CONVERT AUTO POLYGONS TO RASTER

  cl <- parallel::makeCluster(20)
  doParallel::registerDoParallel(cl)
  
  x <- foreach(year = 1984:2019, 
               .combine = 'rbind',
               .packages = c("sf","raster","fasterize")) %do% {
                 
    gl <- read_sf(paste0("C:/Users/bevin/Desktop/Glacier_Outlines_20200613_0914_0.8_1.8_0.4/",year,"_finalPoly_brns_smooth_1.8.sqlite"))
    bbox <- gl %>% sf::st_bbox() %>% st_as_sfc() %>% st_as_sf() %>% as_Spatial()
    gl_ras <- fasterize::fasterize(sf = gl, raster = raster(bbox, res= 30))
    raster::writeRaster(x = gl_ras, 
                        filename = paste0("C:/Users/bevin/Desktop/Glacier_Outlines_20200613_0914_0.8_1.8_0.4/",year,"_finalPoly_brns_smooth_1.8.tif"), 
                        datatype = "INT1U", 
                        overwrite = T)}
  
  stopCluster(cl)

# CONVERT BOLCH 1985 POLYGONS TO RASTER

  gl <- read_sf("../../2018_RGI2/manuscript_outlines/Bolch/glacinvent_1985/bolch_1985_clean.shp")
  bbox <- gl %>% sf::st_bbox() %>% st_as_sfc() %>% st_as_sf() %>% as_Spatial()
  gl_ras <- fasterize::fasterize(sf = gl, raster = raster(bbox, res= 30))
  raster::writeRaster(x = gl_ras, 
                      filename = paste0("C:/Users/bevin/Desktop/Glacier_Outlines_20200613_0914_0.8_1.8_0.4/bolch_1985_clean.tif"), 
                      datatype = "INT1U", 
                      overwrite = T)

# CONVERT BOLCH 2005 POLYGONS TO RASTER

  gl <- read_sf("../../2018_RGI2/manuscript_outlines/Bolch/glacinvent_2005/glacinvent_2005_poly_final.shp")
  bbox <- gl %>% sf::st_bbox() %>% st_as_sfc() %>% st_as_sf() %>% as_Spatial()
  gl_ras <- fasterize::fasterize(sf = gl, raster = raster(bbox, res= 30))
  raster::writeRaster(x = gl_ras, 
                      filename = paste0("C:/Users/bevin/Desktop/Glacier_Outlines_20200613_0914_0.8_1.8_0.4/bolch_2005_clean.tif"), 
                      datatype = "INT1U", 
                      overwrite = T)