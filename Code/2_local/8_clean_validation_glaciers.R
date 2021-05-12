library(dplyr)
library(stringr)
library(smoothr)
library(doParallel)
library(foreach)    
library(sf)
library(stars)

my_val_pnt <- read_sf("2_Paper_BCA_Glacier/0_Data/Validation_Glaciers/my_val_glaciers.sqlite")
my_val_pol <- read_sf("2_Paper_BCA_Glacier/0_Data/Validation_Glaciers/my_val_glaciers_poly.shp")
fol <- "E:/Google Drive/GlacierChangeBC_Paper_rgee_validation"
dirs <- list.dirs(fol, recursive = F, full.names = T)

# for(my_name in my_val_pnt$name){
cl <- parallel::makeCluster(parallel::detectCores())
doParallel::registerDoParallel(cl)
  # all(my_glaciers1985[1,])
  # foreach(i = glids) %do% print(i)
  # df2$my[2:length(df2$my)]-1
  x = foreach(j = 1:nrow(my_val_pnt),
              .combine = 'rbind',
              .packages = c("stars","sf","dplyr","smoothr","stringr"),
              .export = c("my_val_pnt","my_val_pol","fol","dirs")
  ) %dopar% {
j=2
    source("2_Paper_BCA_Glacier/2_R_Code/2_local/STARS_GLACIER_FUNCTIONS.R")  
  my_name  <- my_val_pnt[j,]$name  
  print(my_name)
  my_pnt <- my_val_pnt %>% dplyr::filter(name == my_name) %>% dplyr::summarise()
  my_pol <- my_val_pol[my_val_pol %>% st_intersects(my_pnt, sparse = F),] %>% st_make_valid()
  # my_name <- my_pnt$name
  my_id <- my_pol$Id
  
  ras_list <- list.files(dirs[grep(pattern = my_id,x = dirs)], pattern = "*.tif$")
  date_list <- str_split_fixed(string = ras_list, "_", n = 9)[,5]
  ras_list <- list.files(dirs[grep(pattern = my_id,x = dirs)], full.names = T, pattern = "*.tif$")
  
  bands <- c("b","g","r","n","s1","s2","t","ndwi","brns","brrs","brns2","brrs2")
  
  # i <- 1
  b <- "brns"
  t <- 1.8
  
  my_auto <- do.call(rbind, lapply(1:length(ras_list), function(i){
    print(paste(i, "of", length(ras_list)))
    r <- read_stars(ras_list[i])
    d <- date_list[i]
    y <- format(as.Date.character(date_list[i], format = "%Y%m%d"), format = "%Y")
    r <- st_set_dimensions(r, which = 3, values = bands, names = "bandname")
    r <- r[my_pol %>% st_transform(st_crs(r))]
    brns <- r %>% filter(bandname == b)
    brns_th <- brns>t
    brns_th_pol <- brns_th %>% st_as_sf(merge = T) 
    names(brns_th_pol) <- c("val","geometry")
    brns_th_pol <- brns_th_pol %>% filter(val == 1)
    
    brns_th_pol <- brns_th_pol %>% group_by(val) %>% summarise() %>%
      mutate(year = y)
    
    
    brns_th_pol_clean <- cleanBR(ref_glacier = my_pol, my_poly_to_clean = brns_th_pol %>% st_transform(3005))
    brns_th_pol_clean_final <-  final_clean(brns_th_pol_clean)
    brns_th_pol_clean_final <- brns_th_pol_clean_final %>% 
      mutate(Id = my_id,
             date = d,
             year = y,
             source = b, 
             name = my_name,
             landsat_id = list.files(dirs[grep(pattern = my_id,x = dirs)], pattern = "*.tif$")[i])
    return(brns_th_pol_clean_final)}))
  
  file.remove(paste0(fol,"/",my_name,"_",my_id,"_val.sqlite"))
  write_sf(my_auto, paste0(fol,"/",my_name,"_",my_id,"_val.sqlite"))
  print(my_name)
  print("complete")
  }
  
