#### Functions (Raster) ####

# Function to Scale Surface Reflectance Values (srScale) 

srScale = function(img){
  img$
    addBands(img$select(c('blue', 'green', 'red', 'nir', 'swir1', 'swir2'))$multiply(0.0001))$
    addBands(img$select(c('tir'))$multiply(0.1))$
    select(c('blue_1', 'green_1', 'red_1', 'nir_1', 'swir1_1', 'swir2_1','tir_1'),
           c('blue', 'green', 'red', 'nir', 'swir1', 'swir2','tir'))}

# Function to Mask Radiometric Errors of Value 2 (radiometric)  

radiometric = function(img){
  blue = img$select('blue')$eq(2);
  blueAdd = img$select('blue')$subtract(blue);
  green = img$select('green')$eq(2);
  greenAdd = img$select('green')$subtract(green);
  red = img$select('red')$eq(2);
  redAdd = img$select('red')$subtract(red);
  img$addBands(blueAdd)$addBands(greenAdd)$addBands(redAdd)$select(c('blue_1', 'green_1', 'red_1', 'nir', 'swir1', 'swir2','tir'), ST_NAMES)};

# Function to Mask Clouds (cloudMask) 

cloudMask = function(img){
  temp  = img$addBands(img$select('tir')$unitScale(240,270)); 
  temp  = temp$addBands(temp$normalizedDifference(c('tir_1','swir2'))$rename('ndci'));
  temp  = temp$addBands(temp$select('ndci')$lte(cloudThresh)$rename('ndciT'));
  mask  = temp$select('ndciT')$fastDistanceTransform(51, 'pixels', 'squared_euclidean')$sqrt()$multiply(ee$Image$pixelArea()$sqrt())$gt(cloudBuff);
  img$updateMask(mask)};

# Function to Calculate Pixel Area (pixelArea) 

pixelArea2 = function(img){
  img$multiply(ee$Image$pixelArea())$divide(1000*1000)};

# Function to Compute Glacier and Snow (computeGlaciersAndSnow)  

computeGlaciersAndSnow = function(img){
  
  # img = img$clipToCollection(glaciers);
  
  ndwi = img$
    updateMask(slp$lt(20))$
    normalizedDifference(c("green","nir"))$
    rename(c('ndwi'));     
  
  brns = img$
    select(c("nir"))$divide(img$select(c("swir1")))$
    rename(c('brns'));     
  
  brrs = img$
    select(c("red"))$divide(img$select(c("swir1")))$
    rename(c('brrs'));     
  
  brns2 = img$
    select(c("nir"))$divide(img$select(c("swir2")))$
    rename(c('brns2'));     
  
  brrs2 = img$
    select(c("red"))$divide(img$select(c("swir2")))$
    rename(c('brrs2'));     
  
  img$float()$
    addBands(ndwi)$float()$
    addBands(brns)$float()
    # addBands(brrs)$float()$
    # addBands(brns2)$float()$
    # addBands(brns2)$float()
    }

# Function to Compute Glacier and Snow (computeGlaciersAndSnow)  

computeGlaciersAndSnow_threshold = function(img){
  
  complete = img$
    select('nir')$
    gt(-10000000)$
    rename('complete'); 
  
  ndwi_th = img$
    select('ndwi')$
    gt(waterThresh)$
    unmask()$
    rename(c('ndwi_th'));  
  
  ndwi_th_km = ndwi_th$
    addBands(pixelArea2(ndwi_th))$
    reduceConnectedComponents(ee$Reducer$sum(),"ndwi_th")$
    gt(pixThreshWa)$
    unmask()$
    rename(c('ndwi_th_km')); 
  
  ndwi_th_km_out = ndwi_th$
    subtract(ndwi_th_km)$
    rename(c('ndwi_th_km_out')); 
  
  brns_th = img$
    select('brns')$
    gt(bandThresh)$
    subtract(ndwi_th_km)$
    eq(1)$
    unmask()$
    rename(c('brns_th'));     
  
  brns_th_km = brns_th$
    addBands(pixelArea2(brns_th))$
    reduceConnectedComponents(ee$Reducer$sum(),"brns_th")$
    lt(pixThreshGl)$
    unmask()$
    rename(c('brns_th_km')); 
  
  brns_th_km_out = brns_th$
    subtract(brns_th_km)$
    eq(1)$
    rename('brns_th_km_out');
  
  snow_nir_th = img$
    select('nir')$
    gt(0.4)$
    rename('snow_nir_th')
  
  snow_nir_th_km = snow_nir_th$
    addBands(pixelArea2(snow_nir_th))$
    reduceConnectedComponents(ee$Reducer$sum(),"snow_nir_th")$
    lt(pixThreshSn)$
    rename(c('snow_nir_th_km'));
  
  snow_nir_th_km_out = snow_nir_th$
    subtract(snow_nir_th_km)$
    eq(1)$
    updateMask(brns_th_km)$
    selfMask()$
    rename("snow_nir_th_km_out");
  
  img$
    addBands(complete)$float()$
    addBands(ndwi_th)$float()$
    addBands(ndwi_th_km)$float()$
    addBands(ndwi_th_km_out)$float()$
    addBands(brns_th)$float()$
    addBands(brns_th_km)$float()$
    addBands(brns_th_km_out)$float()$
    addBands(snow_nir_th)$float()$
    addBands(snow_nir_th_km)$float()$
    addBands(snow_nir_th_km_out)$float()}

# Function to Map Results over a List of Years to Map computeGlaciersAnd Snow (resultFunction) 

resultFunction = function(year){
  
  begin = ee$Date$fromYMD(year, 1, 1)
  end = begin$advance(search, 'year')
  computeGlaciersAndSnow(begin, end)}

setYear = function(ft){
  ft$set("year", img$get("year"))}

myPercentile = function(year){
  # base = ee$Image(-9)$clip(aoi)
  # year = 2012
  begin = ee$Date$fromYMD(year, 1, 1);
  end = begin$advance(loop_year, 'year');
  
  colSub = col$
    filterDate(begin,end)
  
  colsize = colSub$size()$getInfo()
  
  if(colsize > 0){
    mosaic = colSub$
      reduce(ee$Reducer$percentile(list(percentile)))$
      rename(c('blue','green','red','nir','swir1','swir2','tir','ndwi','brns'))$
      set('year', begin)
    # print(paste("Total Landsat Images:", colsize))
    # mosaic
  }else{
    mosaic = ee$ImageCollection(ee$Image(-9)$addBands(-9)$addBands(-9)$addBands(-9)$addBands(-9)$addBands(-9)$addBands(-9)$addBands(-9)$addBands(-9)$
                                  float()$
                                  clip(aoi)$
                                  rename(c('blue','green','red','nir','swir1','swir2','tir','ndwi','brns')))$
      first()$
      set('year', begin)
    # print(paste("Total Landsat Images:", colsize))
    # maxValue(mosaic$select("ndwi"))$get("ndwi")$getInfo()
  }
  # ee_print(mosaic)
  
  # Map$addLayer(mosaic9$min())
  # ee_print(mosaic9)
  # ee_print(mosaic)
  
  # merge = ee$ImageCollection(mosaic$select('ndwi'))$merge(
  #         ee$ImageCollection(ee$Image(-9,-9,-9,-9,-9,-9,-9,-9,-9)$
  #                            rename(c('blue','green','red','nir','swir1','swir2','tir','ndwi','brns'))))
  return(mosaic)
}

myclip_aoi = function(img){
  img$clip(aoi)}


# get highest value

maxValue <- function(img, scale = 30) {
  img$reduceRegion(
    reducer = ee$Reducer$max(),
    geometry = aoi,
    scale = scale,
    maxPixels = 1e13
  )
}


# Function to Sum Glacier and Snow Pixels per Glacier (glacierExtract) 

# glacierExtract = function(img){
#     #gl_1 = ee$FeatureCollection(pixelArea2(img$select(c("out_complete","out_br_m","out_snowMsk_nir","out_water_m")))
#     gl_1 = ee$FeatureCollection(pixelArea2(img$select(c("out_complete","out_br_m")))\
#                                 $reduceRegions({'reducer': ee$Reducer$sum(), 'crs': crsSet, 'collection': glaciers, 'scale': imageRes, 'tileScale': tileScaleSet}));
#     #           gl_2 = ee$FeatureCollection(dem$updateMask(img$select(c('out_snowMsk_nir')))
#     #                         $reduceRegions({collection: gl_1, reducer: ee$Reducer$percentile(c(0,10), c("ter_snow_p00","ter_snow_p10")), scale: imageRes, crs: crsSet, tileScale: tileScaleSet}));
#     gl_3 = ee$FeatureCollection(dem$updateMask(img$select(c('out_br_m')))\
#                                 $reduceRegions({'collection': gl_1, 'reducer': ee$Reducer$percentile(c(0,10), c("ter_glacier_p00","ter_glacier_p10")), 'scale': imageRes, 'crs': crsSet, 'tileScale': tileScaleSet}));
#     #$reduceRegions({collection: gl_2, reducer: ee$Reducer$percentile(c(0,10), c("ter_glacier_p00","ter_glacier_p10")), scale: imageRes, crs: crsSet, tileScale: tileScaleSet}));
#     gl_4 = ee$FeatureCollection(pixelArea2(debris)\
#                                 $reduceRegions({'collection' : gl_3, 'reducer': ee$Reducer$sum(), 'crs': crsSet, 'scale': imageRes, 'tileScale': tileScaleSet}));
#     gl_4\
#             $map(Fgeom)$map(Fparameters)$map(setYear);