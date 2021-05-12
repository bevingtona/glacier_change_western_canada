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

ee_Initialize(email = "bevingtona@gmail.com", drive = TRUE)

# READ FUNCTIONS #### 

  source("Code/1_rgee/f_rasters.R")
  source("Code/1_rgee/f_vectors.R")
  source("Code/1_rgee/f_terrain.R")

# SET AOI ####

  # OPTIONAL: Read Regions
  # regions <- read_sf("../../2018_RGI2/manuscript_outlines/Regions/constrained.shp")

  # Read 1985 glacier inventory
  glaciers <- read_sf("../../2018_RGI2/manuscript_outlines/Bolch/glacinvent_1985/bolch_1985_clean.shp") %>% st_transform(4326)

  # OPTIONAL: Subset glaciers
  glaciers <- glaciers %>% filter(area > 14.5 & area < 14.6)
  
  # AOI BBOX 
  regions_bbox <- st_as_sfc(st_bbox(glaciers))
  
  # AOI_GRID
  regions_grid <- st_make_grid(regions_bbox, cellsize =  c(0.2, 0.2), crs = 4326, what = 'polygons') %>%
    st_sf('geometry' = ., data.frame('ID' = 1:length(.)))
  
  # INTERSECT AOI GRID WITH GLACIERS
  regions_grid_filter <- regions_grid[st_intersects(regions_grid, glaciers, sparse = F),]
  
  # CHECK_GRID
  mapview(regions_grid) + mapview(glaciers)
  
  # WRITE_READ
  write_sf(regions_grid_filter, paste0("Data/regions_grid_filter", nrow(regions_grid_filter),".sqlite"))
  # regions_grid_filter <- read_sf("regions_grid_filter.sqlite")
  
# RUN WORKFLOW PER GRID #### 
  
  for(grid in 1:nrow(regions_grid_filter)){
    
    print(grid)

    source("Code/1_rgee/1_imports.R")
    
    loop_minY = 1984
    loop_maxY = 2020
    folder = "GlacierChangeBC_Paper_rgee_2020_ex";
    loop_search = 1
    bbox <- regions_grid_filter[grid,] %>% st_bbox()
    reg_name <- regions_grid_filter[grid,]$id #"bc"
    aoi <- ee$Geometry$Rectangle(c(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]]))
    glaciers <-  glaciers$filterBounds(aoi)
    
    print(reg_name)
    
    source("Code/1_rgee/2_parameters.R")
    source("Code/1_rgee/3_load_collection.R")
    source("Code/1_rgee/4_mosaic.R")
    }