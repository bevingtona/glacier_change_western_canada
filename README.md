# Glacier area change in British Columbia and Alberta

This is a code repository for the journal article: **Accelerated change in the glaciated environments of western Canada revealed through trend analysis of optical satellite imagery**. 

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
    -   5_smooth_tif_to_poly_per_glaciers.R

-   Analysis and Figures
    -   Load_data.R
    -   Figure_3.R
    -   Figure_5.R
    -   Figure_6.R
    -   Figure_7.R
    -   Figure_8.R
    -   Figure_9.R
    -   Figure_10.R
    -   Figure_11_era_precip_plots.R
    -   Figure_11_era_temp_plots.R
    -   Figure_13.R
