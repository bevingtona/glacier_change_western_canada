library(sf)
library(stars)
library(dplyr)

library(doParallel)
library(foreach) 

cl <- parallel::makeCluster(parallel::detectCores())
doParallel::registerDoParallel(cl)

x <- foreach(year = 1996:2019,
             .combine = 'rbind',
             .packages = c("sf","stars","dplyr")) %dopar% {

  outfolder <- "C:/Users/bevin/Desktop/2020-06-17_clipToCol_30_50_210_260_smspline_06_18_04_005_001_005_001/2_smooth_polys/"
  # year = 2000
  file.remove(paste0(outfolder,year,"_finalPoly_brns_smooth_1.8.gpkg"))
               
               # year = 1994
  tif <- read_stars(list.files(path = outfolder, 
                      pattern = paste0(year,"_finalPoly_brns_smooth_1.8.tif"), 
                      recursive = F, 
                      full.names = T))


  my_glaciers1985 <- read_sf("../../2018_RGI2/manuscript_outlines/Bolch/glacinvent_1985/bolch_1985_clean.shp") %>% 
    mutate(disolve_n = row_number(), area_new = st_area(.))


                 
  lapply(1:nrow(my_glaciers1985), function(i){
    write.csv("", paste0(outfolder,"status_folder2/",year,"_",i,".csv"))
    # i=5
    my_glacier <- my_glaciers1985[i,] %>% st_make_valid() %>% st_transform(st_crs(tif))
    my_tif <- (st_as_stars(tif[my_glacier]) == 1)
    st_crs(my_tif) <- 3005
    # plot(tif[my_glacier])
    # INTERSECTION (CLIP)
    my_glacier_year_inters <- my_tif %>% st_as_sf() 
  
    if(nrow(my_glacier_year_inters) != 0){
      
      my_glacier_year_inters_buf <- my_glacier_year_inters %>% 
        # st_transform(3005) %>% 
        dplyr::summarise() %>% 
        st_intersection(my_glaciers1985[i,] %>% st_make_valid()) %>% 
        st_make_valid()%>%
        st_buffer(1) %>%
        st_union() %>%
        st_sf() %>%
        st_cast("POLYGON") %>% 
        summarise(n_parts = n())
      
      my_glacier_year_inters_buf_u_sf_n_out <-
        my_glacier_year_inters_buf %>%
        mutate(year = year,
               ID_FS = my_glacier$ID_FS,
               name = my_glacier$name,
               GLIMS_ID = my_glacier$GLIMS_ID,
               WC2N_ID = my_glacier$WC2N_ID,
               region = my_glacier$region,
               disolve_n = my_glacier$disolve_n,
               area_1985 = round(my_glacier$area_new,0),
               area_Auto = round(st_area(.),0))
      sf::st_write(my_glacier_year_inters_buf_u_sf_n_out, 
                   paste0(outfolder,year,"_finalPoly_brns_smooth_1.8.gpkg"), 
                   append = T, quiet = T)
      
    }
    file.remove(paste0(outfolder,"status_folder2/",year,"_",i,".csv"))
    
    })
             
}
stopCluster(cl)

# d <- st_read(paste0("C:/Users/bevin/Desktop/Glacier_Outlines_20200613_0914_0.8_1.8_0.4/",2018,"_finalPoly_brns_smooth_1.8.gpkg"))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # # cl <- parallel::makeCluster(20)
  # # doParallel::registerDoParallel(cl)
  # 
  # x <- foreach(year = 1984:2019, 
  #              .combine = 'rbind',
  #              .packages = c("sf","raster","fasterize")) %do% {
  #                
  #                gl <- read_sf(paste0("C:/Users/bevin/Desktop/Glacier_Outlines_20200613_0914_0.8_1.8_0.4/",year,"_finalPoly_brns_smooth_1.8.sqlite"))
  #                bbox <- gl %>% sf::st_bbox() %>% st_as_sfc() %>% st_as_sf() %>% as_Spatial()
  #                gl_ras <- fasterize::fasterize(sf = gl, raster = raster(bbox, res= 30))
  #                raster::writeRaster(x = gl_ras, 
  #                                    filename = paste0("C:/Users/bevin/Desktop/Glacier_Outlines_20200613_0914_0.8_1.8_0.4/",year,"_finalPoly_brns_smooth_1.8.tif"), 
  #                                    datatype = "INT1U", 
  #                                    overwrite = T)}
  # 
  # 
  # # stopCluster(cl)
