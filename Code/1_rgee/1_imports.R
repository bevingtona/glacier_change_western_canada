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
