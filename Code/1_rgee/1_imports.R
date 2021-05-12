###############################################################################
# Alexandre Bevington                                                         
# Research Hydrologist, BC Government, FLNRORD, alexandre.bevington@gov.bc.ca 
# PhD Candidate, UNBC, bevington@unbc.ca                                      
###############################################################################
#
# This script imports datasets into Google Earth Engine. 
# It is called from the "5_runitall.R" file
# 
###############################################################################
#
# 2020-06-15
#
###############################################################################

#### Imports ####

  # Sentinel-2 Debris cover (Scherler et al 2018)
  
    debris_rgi1_scherler = ee$FeatureCollection("users/bevingtona/01_rgi60_Alaska_S2_DC_2015_2017_NDSI");
    debris_rgi2_scherler = ee$FeatureCollection("users/bevingtona/02_rgi60_WesternCanadaUS_S2_DC_2015_2017_NDSI");
    debris = ee$Image(1)$clipToCollection(debris_rgi1_scherler$merge(debris_rgi2_scherler));

  # Glacier inventory (Bolch et al 2010)

    glaciers_1985 = ee$FeatureCollection("users/bevingtona/bolch_1985_clean_v2")
    glaciers = glaciers_1985

  # DEM (CDED priority, fill with SRTM 30m)

    srtm = ee$Image('USGS/SRTMGL1_003')$select('elevation')
    srtm_hs = ee$Terrain$hillshade(srtm)
    
    cded = ee$ImageCollection('NRCan/CDEM')$select('elevation')$mosaic()
    cded_hs = ee$Terrain$hillshade(cded)
    
    dem = cded$unmask()$max(srtm$unmask())
    slp = ee$Terrain$slope(dem)$rename(c('slope'));
    asp = ee$Terrain$aspect(dem)$rename(c('aspect'));
    dem_hs = ee$Terrain$hillshade(dem)
    
    demName = "srtm+cded"  

  # Import Regions

    # bc = ee$FeatureCollection("users/bevingtona/BC")$geometry()$bounds()$buffer(200000)
    # regions = ee$FeatureCollection('users/bevingtona/us_canada_glacier_regions')$filterBounds(bc)


    