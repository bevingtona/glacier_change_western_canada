# Glacier area change in British Columbia and Alberta

This is a code repository for the journal article: **Accelerated change in the glaciated environments of western Canada revealed through trend analysis of optical satellite imagery**. https://www.sciencedirect.com/science/article/pii/S0034425721005824

The code is organized in three sections: 

-   Google Earth Engine Functions
    -   f_rasters.R
    -   f_terrain.R
    -   f_vectors.R

-   Google Earth Engine Workflow
    -   1_imports.R
    -   2_parameters.R
    -   3_load_collection.R
    -   4_mosaic.R

-   Local processing
    -   1_vrt.R
    -   2_vrt_to_smooth.R
    -   3_smooth_tile_to_vrt.R
    -   4_smooth_vrt_to_poly.R
    -   5_join_polys.R
 

## EXAMPLE: How to read .GPKG that is available from Zenodo using R:


```
library(sf)
file <- "bca_glaciers_1984-2021.gpkg"
st_layers(file)
dat <- st_read(file, layer = "new_all_join_id_filter_20220120-0835", query = "select * from `new_all_join_id_filter_20220120-0835` where year = 2000 limit 1")
print(dat)
mapview::mapview(dat)
```
