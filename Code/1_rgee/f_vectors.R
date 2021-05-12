
#### Functions (Vector) ####

  # Function to Remove Geometry Column from Table (i$e$ faster export) (Fgeom)
  
    Fgeom = function(feature){
      ee$Feature(feature$select(c("$*"), null, false))};
  
  # Function to Add a Buffer to the Glaciers (bufferGlacier) 
  
    bufferGlacier = function(ft){
      ft$buffer(glacierBuff, ee$ErrorMargin(error))};
  
  # Function to Add and Area Column a Feature (addArea) 
  
    addArea = function(feature){
      ee$Algorithms$ProjectionTransform(feature, crsSet, ee$ErrorMargin(error))$
        set(areaKm2, feature$geometry(1)$area(1)$divide(1000 * 1000))$
        set(name, feature$get('system:index'))};
  
  # Function to Add Columns to Output (Fparameters) 
  
    Fparameters = function(feature){feature$
        set('proc_clouds',clouds)$
        set('proc_imageRes',imageRes)$
        set('proc_doyStart',doyStart)$
        set('proc_doyEnd',doyEnd)$
        set('proc_percentile',percentile)$
        set('proc_search',search)$
        set('proc_minY',minY)$
        set('proc_maxY',maxY)$
        set('proc_incY',incY)$
        set('proc_bandThresh',bandThresh)$
        set('proc_blueThresh', blueThresh)$
        set('proc_waterThresh',waterThresh)$
        set('proc_ndsiSnThresh',ndsiSnThresh)$
        set('proc_ndsiGlThresh',ndsiGlThresh)$
        set('proc_cloudThresh',cloudThresh)$
        set('proc_cloudBuff',cloudBuff)$
        set('proc_glacierBuff',glacierBuff)$
        set('proc_pixThreshGl',pixThreshGl)$
        set('proc_pixThreshSn',pixThreshSn)$
        set('proc_demName', demName)$
        set('proc_comment',comment)$
        set('proc_thresholdName',comment)$
        set('proc_tileScaleSet', tileScaleSet)$
        set('proc_crsSet', crsSet)$
        set('proc_error',error)}
