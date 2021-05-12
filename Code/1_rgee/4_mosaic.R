###############################################################################
# Alexandre Bevington                                                         
# Research Hydrologist, BC Government, FLNRORD, alexandre.bevington@gov.bc.ca 
# PhD Candidate, UNBC, bevington@unbc.ca                                      
###############################################################################
#
# This script creates/downloads annual mosaics of both NDWI and BRNS 
# in Google Earth Engine. 
# It is called from the "5_runitall.R" file
# 
###############################################################################
#
# 2020-06-15
#
###############################################################################


#### NDWI mosaic bands download ####

  print("NDWI")
  result_ndwi = myPercentile(loop_minY)$select("ndwi")$rename(paste0("ndwi_",loop_minY))#$unmask()$clip(aoi)$max(base)
  # print(loop_minY)

  for(y in seq(loop_minY+loop_search,loop_maxY,loop_search)){
    # print(y)
    temp = myPercentile(y)$select("ndwi")$rename(paste0("ndwi_",y))
    result_ndwi = result_ndwi$addBands(temp)}
  # ee_print(result_ndwi)
  task <- ee$batch$Export$image(result_ndwi,
                                paste0(format(lubridate::now(),"%Y%m%d%H%M"),"_",reg_name,"_ndwi_",y,"_clipToCol_",clouds,"_",percentile,"_",doyStart,"_",doyEnd),
                                downConfig)
  task$start()

#### BRNS mosaic bands download ####

  print("BRNS")
  result_brns = myPercentile(loop_minY)$select("brns")$rename(paste0("brns_",loop_minY))#$unmask()$clip(aoi)$max(base)
  # print(loop_minY)

  for(y in seq(loop_minY+loop_search,loop_maxY,loop_search)){
    # print(y)
    temp = myPercentile(y)$select("brns")$rename(paste0("brns_",y))
    result_brns = result_brns$addBands(temp)}
  # ee_print(result_brns)
  task <- ee$batch$Export$image(result_brns,
                                paste0(format(lubridate::now(),"%Y%m%d%H%M"),"_",reg_name,"_brns_",y,"_clipToCol_",clouds,"_",percentile,"_",doyStart,"_",doyEnd),
                                downConfig)
  task$start()

#### NIR mosaic bands download ####
  
  print("NIR")
  result_brns = myPercentile(loop_minY)$select("nir")$rename(paste0("nir_",loop_minY))#$unmask()$clip(aoi)$max(base)
  # print(loop_minY)

  for(y in seq(loop_minY+loop_search,loop_maxY,loop_search)){
    # print(y)
    temp = myPercentile(y)$select("nir")$rename(paste0("nir_",y))
    result_brns = result_brns$addBands(temp)}
  # ee_print(result_brns)
  task <- ee$batch$Export$image(result_brns,
                                paste0(format(lubridate::now(),"%Y%m%d%H%M"),"_",reg_name,"_nir_",y,"_clipToCol_",clouds,"_",percentile,"_",doyStart,"_",doyEnd),
                                downConfig)
  task$start()