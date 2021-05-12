library(dplyr)
library(sf)

# FUNCTIONS 

my_read_write_sf <- function(index){
  # for(i in 1:30){
  # print(i)
  # index <- list[19]
  s <- try(st_read(index, quiet = T), silent = T)
  if(!is.null(nrow(s))){
    if(nrow(s)>0){
      p <- s %>% st_cast("MULTIPOLYGON")
      return(p)
    }
  }
}

# VARIABLE 
brns_name <- "brns_moving_2_18"
ndwi_name <- "ndwi_moving_2_45"

outfolder <- "E:/clipToCol_30_20_210_250/2_smooth_polys_NAfill/"
dirs <- list.dirs(paste0(outfolder,brns_name))

lapply(1:length(dirs), function(x){
  # x=7970
  dir <- dirs[x]
  list <- list.files(path = dir, 
                     recursive = F, 
                     pattern = paste0("_glacier_",brns_name,"_",ndwi_name,".sqlite"), 
                     full.names = T)
  if(length(list)==0){print(x)}else{
    temp <- do.call(rbind,lapply(list, my_read_write_sf))
    if(!is.null(temp)){
    st_write(temp,
             paste0(outfolder,"/glacier2_",brns_name,"_",ndwi_name,".gpkg"),
             layer = paste0("glacier2_",brns_name,"_",ndwi_name), append=TRUE, quiet = T)}
    print(x)}
  })



# list <- list.files(outfolder, pattern = paste0("_glacier_",brns_name,"_",ndwi_name,".sqlite"), full.names = T, recursive = T)
# 
# my_read_write_sf <- function(index){
#   print(index)
#   my_sf <- read_sf(list[index]) %>% st_cast("MULTIPOLYGON")
#   st_write(my_sf, 
#            paste0(outfolder,"/glacier_",brns_name,"_",ndwi_name,".gpkg"), 
#            layer = paste0("glacier_",brns_name,"_",ndwi_name), append=TRUE, quiet = T)}
# 
# lapply(1:length(list), my_read_write_sf)
