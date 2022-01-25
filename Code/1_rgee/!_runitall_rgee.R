###############################################################################
# Alexandre Bevington                                                         
# Research Hydrologist, BC Government, FLNRORD, alexandre.bevington@gov.bc.ca 
# PhD Candidate, UNBC, bevington@unbc.ca                                      
###############################################################################
#
# This script runs it all by tile!
# 
###############################################################################
#
# 2020-06-15
#
###############################################################################

library(bcdata)
library(sf)
library(tidyverse)
library(rgee)
library(bcmaps)
library(mapview)

gdrive <- "E:/Google Drive/"
folder <- "GlacierChangeBC_Paper_rgee_2021"

ee_Initialize(user = "bevingtona@gmail.com", drive = TRUE)

# READ FUNCTIONS #### 

  source("Code/1_rgee/f_rasters.R")
  source("Code/1_rgee/f_vectors.R")
  source("Code/1_rgee/f_terrain.R")

# SET AOI ####

  # Read 1985 glacier inventory from Bolch et al. 2010
  glaciers <- read_sf("../../2018_RGI2/manuscript_outlines/Bolch/glacinvent_1985/bolch_1985_clean.shp")
  
  # Fix invalid geometries
  glaciers <- glaciers %>% st_make_valid()

  # Get the boundig box in WGS84 
  glaciers <- glaciers %>% st_transform(4326)
  regions_bbox <- st_as_sfc(st_bbox(glaciers))
  
  # CREATE A GRID
  regions_grid <- st_make_grid(
    regions_bbox, 
    cellsize =  c(0.2, 0.2), 
    crs = 4326, 
    what = 'polygons') %>%
    st_sf('geometry' = ., data.frame('ID' = 1:length(.)))
  
  # INTERSECT AOI GRID WITH GLACIERS
  regions_grid_filter <- regions_grid %>% filter(apply(st_intersects(., glaciers, sparse = FALSE), 1, any))
  
  # CHECK_GRID
  plot(regions_grid_filter)
  
  # WRITE_READ
  write_sf(regions_grid_filter, paste0("Data/regions_grid_filter", nrow(regions_grid_filter),".sqlite"))
  regions_grid_filter <- read_sf(paste0("Data/regions_grid_filter", nrow(regions_grid_filter),".sqlite"))
  
# RUN WORKFLOW PER GRID #### 
  
  # grid=1
  
  for(grid in 1:nrow(regions_grid_filter)){
  
    print(grid)

    source("Code/1_rgee/1_imports.R")
    
    loop_minY = 1984
    loop_maxY = 2021
    loop_search = 1
    loop_year = 1
    
    bbox <- regions_grid_filter[grid,] %>% st_bbox()
    reg_name <- regions_grid_filter[grid,]$id #"bc"
    aoi <- ee$Geometry$Rectangle(c(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]]))
    glaciers <-  glaciers$filterBounds(aoi)
    
    # print(reg_name)
    
    source("Code/1_rgee/2_parameters.R")
    source("Code/1_rgee/3_load_collection.R")
    source("Code/1_rgee/4_mosaic.R")
  }
  