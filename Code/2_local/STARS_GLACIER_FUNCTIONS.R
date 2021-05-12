#### GLACIER CLEANING FUNCTIONS ####

# clip_raster_to_glacier <- function(my_raster, out_file, out_path, list){
#   if(out_file %in% list){
#       clip_raster <- read_stars(paste(out_path,out_file,sep="/"))
#       print("already exists, file read")}else{
#       clip_raster <- my_raster[mygl_1985]
#       write_stars(clip_raster, paste(out_path,out_file,sep="/"))
#       print("file saved")
#       }
#   clip_raster <- st_as_stars(clip_raster)
#   clip_raster[clip_raster==-9] = NA 
#   return(clip_raster)}
    
#### SMOOTH STACK #### 

# smooth_time_series <- function(my_raster, my_span, out_file, out_path, list){
#   if(out_file %in% list){
#     smooth_raster <- read_stars(paste(out_path,out_file,sep="/"))
#     print("already exists, file read")}else{
#       # my_raster <- st_as_stars(my_raster)
#       # dims <- st_dimensions(my_raster)
#       # vals <- st_get_dimension_values(my_raster, which = 3)
#       my_raster_data <- my_raster[[names(my_raster)]]
#       for(i in 1:dim(my_raster_data)[1]){
#         for(j in 1:dim(my_raster_data)[2]){
#           mydf <- data.frame(year = 1:dim(my_raster_data)[3], val = as.numeric(my_raster_data[i,j,1:dim(my_raster_data)[3]]))
#           if(sum(is.na(mydf$val)) > 8){
#             my_raster_data[i,j,1:dim(my_raster_data)[3]] <- mydf$val}else{
#             my_raster_data[i,j,1:dim(my_raster_data)[3]] <- stats::predict(stats::loess(val ~ year, span = my_span, data = mydf), 1:dim(my_raster_data)[3])}}}
#       my_raster$new <- my_raster_data
#       write_stars(my_raster[2], paste(out_path,out_file,sep="/"))
#       print("file saved")}
#   return(my_raster[2])}
    
#### RASTER TO POLYGON ####

# ras_to_poly_dissolve <- function(i, th, my_raster){
#   r  <-  my_raster[,,,i]>th
#   s <- st_as_stars(r) %>% st_as_sf(as_points = F, merge = F) %>% group_by(V1) %>% summarise()
#   names(s) <- c("val","geometry")
#   s <- s %>% mutate(year = years[i])
#   return(s)}


#### CLEAN WATER POLYGONS ####

cleanerWater <- function(myPolygons=my_ndwi_smooth_pol_y){
  # if(nrow(myPolygons %>% filter(val == 1))>0){
  
    if(nrow(filter(myPolygons, val == 1)) ==0){
      return(myPolygons %>% filter(year == -9))
    }else{
    myPolygons_w <- myPolygons %>%
    group_by(year) %>% 
    filter(val == 1) %>%
    #SPLIT
    st_cast("MULTIPOLYGON") %>%
    st_cast("POLYGON") %>%
    #FILL
    fill_holes(threshold = units::set_units(0.01, km^2)) %>%
    #DROP
    drop_crumbs(threshold = units::set_units(0.01, km^2)) %>%
    #DISSOLVE
    summarise()
  # ungroup() %>%
  # group_by(year) %>%
  # st_cast("MULTIPOLYGON") %>%
  # ungroup() %>%
  #AREA
  # mutate(area_km2 = as.numeric(st_area(.))/(1000*1000)) 
  
  return(myPolygons_w)#}else{myPolygons}
    }
  }



#### CLEAN BAND RAIO #### 

# ref_glacier = glaciers1985[myname,]
# my_poly_to_clean = my_brns_smooth_pol
# my_poly_to_clean %>% filter(year == 2018) %>%  mapview() + mapview(ref_glacier_val)
cleanBR <- function(ref_glacier, my_poly_to_clean){
  st_crs(ref_glacier) <- 3005
  st_crs(my_poly_to_clean) <- 3005
  
  ref_glacier_val <- st_make_valid(ref_glacier) %>% dplyr::select(geometry)
  my_poly_to_clean_val <- st_make_valid(my_poly_to_clean) %>% st_intersection(ref_glacier_val)
  # my_poly_to_clean_val %>% filter(year == 2018) %>%  mapview()
  myPolygons <-
    my_poly_to_clean_val %>% 
    # filter(year == 2019) %>% 
    # filter(val == 0) %>%
    # ungroup() %>% 
    st_cast("MULTIPOLYGON") %>%
    st_cast("POLYGON") %>%
    mutate(area = as.numeric(as.character(st_area(.)/(1000*1000)))) %>%
    filter(area < 0.01 & val ==0 | val == 1) %>% 
    # group_by(year) %>%
    # summarise() %>%
    mutate(val = 1) %>% 
    group_by(year) %>% 
    summarise() 
    # st_sym_difference(ref_glacier_val) %>%
    
    # mapview::mapview() #%>% filter(year == 2018) %>%  mapview()
  
  return(myPolygons)}

#### REMOVE WATER #### 
removeWater <- function(y, my_raster_brns, my_raster_ndwi){
  # y = 2017#
  # print(y)
  if(nrow(my_raster_ndwi)>0){
    water <- st_make_valid(my_raster_ndwi %>% filter(year == y)) %>% st_transform(3005)
    brns_s_s_val <- st_make_valid(my_raster_brns %>% filter(year == y)) %>% st_transform(3005) 
    if(nrow(water)>0){
      out <- st_difference(brns_s_s_val %>% st_buffer(1), water %>% st_buffer(30)) %>% dplyr::select(-year.1)
      }else{
        out <- brns_s_s_val %>% filter(year == y)
    # print(names(out))
  }
  # print(out)
  return(out)
  }else{
      return(my_raster_brns)}
}


#### FINAL CLEAN #### 

final_clean <- function(my_polygons){ 
# my_polygons <- my_brns_smooth_pol_clean_water
myPolygons_filldrop <- my_polygons %>%
  st_buffer(-1) %>%
  #SPLIT
  st_cast("MULTIPOLYGON") %>% 
  st_cast("POLYGON") %>% 
  #FILL 
  fill_holes(threshold = units::set_units(0.01, km^2)) %>% 
  # DROP SLOW
  mutate(area_km2 = as.numeric(st_area(.))/(1000*1000), n = 1) %>% 
  filter(area_km2 > 0.05) %>% 
  #DROP SLOW
  # drop_crumbs(threshold = units::set_units(0.05, km^2)) %>%
  #DISSOLVE
  ungroup() %>%
  group_by(year) %>% 
  summarise(nparts = sum(n)) %>% 
  #AREA 
  mutate(area_km2 = as.numeric(st_area(.))/(1000*1000)) %>% 
  mutate(year = as.numeric(year))
return(myPolygons_filldrop)}




