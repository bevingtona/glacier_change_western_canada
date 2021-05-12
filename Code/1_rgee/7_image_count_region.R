###############################################################################
# Alexandre Bevington                                                         
# Research Hydrologist, BC Government, FLNRORD, alexandre.bevington@gov.bc.ca 
# PhD Candidate, UNBC, bevington@unbc.ca                                      
###############################################################################
#
# This script exports the suitable images metadata pey region
# 
###############################################################################
#
# 2020-06-15
#
###############################################################################

rm(list=ls())

library(bcdata)
library(sf)
library(tidyverse)
library(rgee)
library(bcmaps)

ee_Initialize(email = "bevingtona@gmail.com", drive = TRUE)
rgee::ee_reattach()

source("2_Paper_BCA_Glacier/2_R_Code/1_rgee/f_rasters.R")
source("2_Paper_BCA_Glacier/2_R_Code/1_rgee/f_vectors.R")
source("2_Paper_BCA_Glacier/2_R_Code/1_rgee/f_terrain.R")


my_glaciers <- read_sf("../../2018_RGI2/manuscript_outlines/Bolch/glacinvent_1985/bolch_1985_clean.shp") %>% 
  st_transform(4326) %>% 
  select(region)


my_glaciers$region <- sub("NCMn","NCM",my_glaciers$region)
my_glaciers$region <- sub("SCMs","SCM",my_glaciers$region)


for(reg in unique(my_glaciers$region)){
  print(reg)
  rgee::ee_reattach()
  
  source("2_Paper_BCA_Glacier/2_R_Code/1_rgee/1_imports.R")
  
  bbox <- my_glaciers %>% filter(region == reg) %>% st_bbox()
  aoi <- ee$Geometry$Rectangle(c(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]]))
  
  source("2_Paper_BCA_Glacier/2_R_Code/1_rgee/2_parameters.R")
  
  loop_minY = 1985
  loop_maxY = 2019
  loop_search = 1
  
  source("2_Paper_BCA_Glacier/2_R_Code/1_rgee/3_load_collection.R")
  
  taskParams = list(
    driveFolder =  'GlacierChangeBC_Paper_rgee',
    fileFormat = 'CSV'   # CSV, KMZ, GeoJSON
  )
  
  # export all features in a FeatureCollection as one file
  task <- ee$batch$Export$table(col, reg, taskParams)
  task$start()
  # ee_monitoring()
  }
  
  
  
  
  
  print(reg_name)
  
  source("2_Paper_BCA_Glacier/2_R_Code/1_rgee/2_parameters.R")
  source("2_Paper_BCA_Glacier/2_R_Code/1_rgee/3_load_collection.R")
  source("2_Paper_BCA_Glacier/2_R_Code/1_rgee/4_mosaic.R")
  
  # gc(verbose = T, full = T)
  
}