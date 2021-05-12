###############################################################################
# Alexandre Bevington                                                         
# Research Hydrologist, BC Government, FLNRORD, alexandre.bevington@gov.bc.ca 
# PhD Candidate, UNBC, bevington@unbc.ca                                      
###############################################################################
#
# This script imports and processes Landsat image collections 
# in Google Earth Engine. 
# It is called from the "5_runitall.R" file
# 
###############################################################################
#
# 2020-06-15
#
###############################################################################

#### Import Landsat ####

  L4SR1 = ee$ImageCollection('LANDSAT/LT04/C01/T1_SR');
  L5SR1 = ee$ImageCollection('LANDSAT/LT05/C01/T1_SR');
  L7SR1 = ee$ImageCollection('LANDSAT/LE07/C01/T1_SR')$filterDate('1999-01-01','2003-01-01');
  # L7SR1 = ee$ImageCollection('LANDSAT/LE07/C01/T1_SR')$filterDate('2012-01-01','2013-01-01');
  L8SR1 = ee$ImageCollection('LANDSAT/LC08/C01/T1_SR');

  LT_BANDS = c('B1', 'B2', 'B3', 'B4', 'B5', 'B7', 'B6');
  LE_BANDS = c('B1', 'B2', 'B3', 'B4', 'B5', 'B7', 'B6');
  LC_BANDS = c('B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B10');
  ST_NAMES = c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'tir');

  col = L4SR1$select(LT_BANDS, ST_NAMES)$merge(
        L5SR1$select(LT_BANDS, ST_NAMES))$merge(
        L7SR1$select(LE_BANDS, ST_NAMES))$merge(
        L8SR1$select(LC_BANDS, ST_NAMES))$
    filterBounds(aoi)$
    filterMetadata('CLOUD_COVER','less_than', clouds)$
    filter(ee$Filter$dayOfYear(doyStart,doyEnd))$
    map(srScale)$
    map(radiometric)$
    map(cloudMask)$
    map(computeGlaciersAndSnow)$
    map(myclip_aoi);
  
    print(paste("Total Landsat Images:", 
                col$size()$getInfo()))
    print(col$first()$bandNames()$getInfo())
  
  