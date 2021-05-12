library(gdalUtils)

gdalUtils::gdalbuildvrt(gdalfile = "./2_Paper_BCA_Glacier/0_Data/3_TIF_SMOOTH/brns/*_brns_smooth_0.8.tif", 
                        output.vrt = "./2_Paper_BCA_Glacier/0_Data/3_TIF_SMOOTH/brns.vrt", 
                        separate = 0, 
                        srcnodata = "nan")

gdalUtils::gdalbuildvrt(gdalfile = "./2_Paper_BCA_Glacier/0_Data/3_TIF_SMOOTH/ndwi/*_ndwi_smooth_0.8.tif", 
                        output.vrt = "./2_Paper_BCA_Glacier/0_Data/3_TIF_SMOOTH/ndwi.vrt", 
                        separate = 0, 
                        srcnodata = "nan")

gdalUtils::gdal_translate(src_dataset = "./2_Paper_BCA_Glacier/0_Data/3_TIF_SMOOTH/ndwi.vrt", 
                          dst_dataset = "./2_Paper_BCA_Glacier/0_Data/3_TIF_SMOOTH/ndwi.tif")
