###############################################################################
# Alexandre Bevington                                                         
# Research Hydrologist, BC Government, FLNRORD, alexandre.bevington@gov.bc.ca 
# PhD Candidate, UNBC, bevington@unbc.ca                                      
###############################################################################
#
# This script defines the processing parameters for the annual brns and ndwi 
# mosaics in Google Earth Engine. 
# It is called from the "5_runitall.R" file
# 
###############################################################################
#
# 2020-06-15
#
###############################################################################

#### Define Parameters ####

  # Comments etc$
  
    thresholdName   = "noIteration";
    comment         = "LS"; 
  
  # Mosaic Parameters
  
    # minY            = loop_minY;
    # maxY            = loop_maxY;
    # loop_search     = 1;
    # incY            = 1;
    # years           = ee$List$sequence(minY, maxY, incY);
    imageRes        = 30;
    clouds          = 30; 
    percentile      = 30; #50
    doyStart        = 210; 
    doyEnd          = 250; #260
  
  # Threshold parameters
  
    cloudThresh     = 0.4;
    blueThresh      = 0.1;
    bandThresh      = 1.8; 
    ndsiSnThresh    = 0.93; 
    ndsiGlThresh    = 0.2; 
    waterThresh     = 0.4;  
    cloudBuff       = 1000; #100
    glacierBuff     = 1;
    pixThreshGl     = 0.05;
    pixThreshSn     = 0.01;
    pixThreshWa     = 0.01;
  
  # Processing factors
  
    tileScaleSet    = 16;
    crsSet          = "EPSG:3005";
    error           = 30
    
  # Configure Download 

    downConfig <- list(
      scale = imageRes,
      maxPixels = 1.0E13,
      crs = "EPSG:4326",
      driveFolder = folder,
      region = aoi)

    