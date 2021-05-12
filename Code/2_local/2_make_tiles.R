rm(list=ls())

library(sf)
library(stars)
library(GSIF)

# READ DATA 

my_brns <- read_stars("2_Paper_BCA_Glacier/0_Data/1_GEE_TIF_OUTPUT/brns/brns3005.vrt")
glaciers1985 <- read_sf("../../2018_RGI2/manuscript_outlines/Bolch/glacinvent_1985/bolch_1985_clean_dissolve_pol.shp")

# MAKE TILES
tiles <- getSpatialTiles(as_Spatial(st_as_sf(st_as_sfc(st_bbox(my_brns)))), block.x=5000, block.y=5000, overlap.percent = 1)
tiles_sf <- st_as_sf(tiles)
write_sf(tiles_sf, "2_Paper_BCA_Glacier/0_Data/2_TILES_VECTOR/glaciers1985_5000x5000.gpkg")

# FILTER OVERLAP WITH GLACIERS  

tile_sf <- tiles_sf[glaciers1985, , op = st_intersects]
write_sf(tile_sf, "2_Paper_BCA_Glacier/0_Data/2_TILES_VECTOR/glaciers1985_5000x5000_intersect.sqlite")
