library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# GLACIERS

  glaciers <- ee$FeatureCollection("users/bevingtona/Bolch_1980s_Polygons")

# PARAMS 

  myCRS <- "EPSG:3005"
  myRES <- 30
  diff_thresh <- -10
  
# DEM

  #1980-1990 coast / 1990-2000 Rockies
  cded <- ee$ImageCollection("NRCan/CDEM")$mosaic()$clipToCollection(glaciers)$reproject(crs=myCRS, scale=myRES)
  # 2000
  srtm <- ee$Image("USGS/SRTMGL1_003")$clipToCollection(glaciers)$reproject(crs=myCRS, scale=myRES)
  #2006-2011
  alos <- ee$Image("JAXA/ALOS/AW3D30/V2_2")$select("AVE_DSM")$clipToCollection(glaciers)$reproject(crs=myCRS, scale=myRES)
  
# DEM DIFFERENCE

  srtm_cded <- srtm$subtract(cded)
  # Map$addLayer(srtm_cded, list("min"=-50, "max"=50))
  
  alos_srtm <- alos$subtract(srtm)
  # Map$addLayer(alos_srtm, list("min"=-50, "max"=50))

# TO VECTOR 

  srtm_cded_mask <- srtm_cded$lt(diff_thresh)$selfMask()
  srtm_cded_mask_v = srtm_cded_mask$reduceToVectors(
    geometry= glaciers,
    crs= myCRS,
    scale= myRES,
    geometryType= 'polygon',
    eightConnected= FALSE);
  
  alos_srtm_mask <- alos_srtm$lt(diff_thresh)$selfMask()
  Map$addLayer(alos_srtm_mask)
  alos_srtm_mask_v = alos_srtm_mask$reduceToVectors(
    geometry= glaciers,
    crs= myCRS,
    scale= myRES,
    geometryType= 'polygon',
    eightConnected= FALSE);

# EXPORT RASTER 

  downConfig <- list(scale = myRES, maxPixels = 1.0E13, driveFolder = "GlacierDEM")
  # task <- ee$batch$Export$image(cded, "cded", downConfig); task$start()
  # task <- ee$batch$Export$image(srtm, "srtm", downConfig); task$start()
  # task <- ee$batch$Export$image(alos, "alos", downConfig); task$start()
  # task <- ee$batch$Export$image(srtm_cded, "srtm_cded", downConfig); task$start()
  # task <- ee$batch$Export$image(alos_srtm, "alos_srtm", downConfig); task$start()

# EXPORT VECTOR
  
  taskParams <- list(driveFolder = "GlacierDEM", fileFormat = "GeoJSON")
  # task <- ee$batch$Export$table(srtm_cded_mask_v, paste0("srtm_cded_mask_v_",diff_thresh), taskParams); task$start()
  task <- ee$batch$Export$table(alos_srtm_mask_v, paste0("alos_srtm_mask_v_",diff_thresh), taskParams); task$start()
