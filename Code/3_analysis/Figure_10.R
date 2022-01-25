library(future.apply)  
library(circular)
library(ggtext)
library(patchwork)
library(broom)
library(tictoc)
library(foreach)
library(doParallel)
library(raster)
library(stars)

#### TERRAIN FUNCTION ####

my_ter <- function(i, y){
  
    if(i %in% seq(1,15000,10)){
    write.csv("", paste0("terprogress",i,"_", y, "_2.csv"))}
  
  dem <- raster("G:/Data_DEM/DEM_BC_MOSAIC_4GEE/4demMin_dem.vrt")
  
  gl <- temp %>% filter(n == i) %>% st_transform(crs(dem))
  
  if(!st_is_empty(gl)){
  
    my_dem <- dem %>% crop(gl) %>% mask(gl)
    
    names(my_dem) <- "elevation"
    my_dem <- stack(my_dem, 
                    raster::terrain(my_dem, opt=c('slope'), unit='degrees'),
                    raster::terrain(my_dem, opt=c('aspect'), unit='degrees'))
    gl <- gl %>%
      sf::st_drop_geometry() %>% 
      dplyr::mutate(
        year = y,
        dem_min = my_dem$elevation %>% raster::getValues() %>% min(na.rm=T),
        dem_max = my_dem$elevation %>% raster::getValues() %>% max(na.rm=T),
        dem_avg = my_dem$elevation %>% raster::getValues() %>% mean(na.rm=T),
        slp_min = my_dem$slope %>% raster::getValues() %>% min(na.rm=T),
        slp_max = my_dem$slope %>% raster::getValues() %>% max(na.rm=T),
        slp_avg = my_dem$slope %>% raster::getValues() %>% mean(na.rm=T),
        # asp_min = min(circular(raster::getValues(my_dem$aspect), units = "degrees"), na.rm = T),
        # asp_avg <- 360 %% atan2(cos((my_dem$aspect * pi) / 180) %>% raster::getValues() %>% min(na.rm=T), 
        #                                   sin((my_dem$aspect * pi) / 180) %>% raster::getValues() %>% min(na.rm=T)) * (180/3.141592654),
        # asp_max = max(circular(raster::getValues(my_dem$aspect), units = "degrees"), na.rm = T),
          # asp_avg <- 360 %% atan2(cos((my_dem$aspect * pi) / 180) %>% raster::getValues() %>% max(na.rm=T), 
          #                                 sin((my_dem$aspect * pi) / 180) %>% raster::getValues() %>% max(na.rm=T)) * (180/3.141592654),
        # asp_min = min(raster::getValues(my_dem$aspect), na.rm = T),
        # asp_max = max(raster::getValues(my_dem$aspect), na.rm = T),
        asp_avg = mean(circular(raster::getValues(my_dem$aspect), units = "degrees"), na.rm = T))
    
    if(i %in% seq(1,15000,10)){
      file.remove(paste0("terprogress",i,"_", y, "_2.csv"))}
    
    
    gl %>% 
      mutate(slp_min = ifelse(is.finite(slp_min), slp_min, NA_real_),
             slp_max = ifelse(is.finite(slp_max), slp_max, NA_real_),
             slp_avg = ifelse(!is.na(gl$slp_avg), slp_avg, NA_real_),
             asp_avg = ifelse(!is.na(gl$asp_avg), asp_avg, NA_real_)) 
          # asp_avg <- 360 %% atan2(cos((my_dem$aspect * pi) / 180) %>% raster::getValues() %>% mean(na.rm=T), 
          #                                 sin((my_dem$aspect * pi) / 180) %>% raster::getValues() %>% mean(na.rm=T)) * (180/3.141592654))
  }
}

#### CALCULATE TERRAIN STATS #### 

for(y in 1985:2019){
    
    print(y)
    
    temp <- new_all_join_id_filter %>% filter(year == y) %>% 
      dplyr::select(year, n, method) %>% 
      st_set_crs(3005)
    
    plan(multisession, workers = 35)
    
    results = do.call(bind_rows, 
                      future_lapply(sort(temp$n), function(i){
                        t <- my_ter(i, y)
                        # print(t)
                        return(t)})
                      )
    
    future:::ClusterRegistry("stop")
    
    write.csv(results, paste0("Data/terrain_stats_", y, "_minmax_v2.csv"))
    
    }


#### READ TERRRAIN ####

all_ter <- do.call(rbind, lapply(1984:2020, function(y){
  read.csv(paste0("Data/terrain_stats_", y, "_minmax_v2.csv"))})) %>% as_tibble()

#### PLOT DEM #### 

new_all_ter <- full_join(new_all_join_id_filter, 
                         all_ter)


# new_all_ter %>% 
#   st_drop_geometry() %>% 
#   filter(year == 1984) %>% 
#   ggplot() + 
#   geom_point(aes(area_glacier_km2, dem_avg)) + 
#   scale_x_log10()

new_all_ter_cent <- new_all_ter %>% 
  st_centroid()

new_all_ter_cent_reg <- new_all_ter_cent %>% 
  st_drop_geometry() %>% 
  # filter(n<1000) %>% 
  group_by(n) %>% 
  mutate(year_group = case_when(year <= 2010 ~ "1984-2010", 
                                year >2010 ~ "2011-2020")) %>%
  dplyr::select(year_group, year, n, nparts, area_glacier_km2, area_water_km2, dem_avg, asp_avg) %>% 
  pivot_longer(cols = c("nparts", "area_glacier_km2", "area_water_km2", "dem_avg", "asp_avg")) %>% 
  group_by(year_group, n, name) %>% 
  filter(!is.na(value)) %>% 
  do(tidy = tidy(lm(value ~ year, data = .)))%>%
  unnest(tidy) %>% 
  filter(term == "year")

# full_join(
#   new_all_ter_cent %>% filter(year == 1984) %>% dplyr::select(n, geom),
#   new_all_ter_cent_reg %>% 
#     filter(!is.na(statistic)) %>% 
#     dplyr::select(year_group, n, name, estimate) %>% 
#     pivot_wider(names_from = year_group, 
#                 values_from = estimate) %>% 
#     mutate(dif = `2011-2020`-`1984-2010`)) %>% 
#   filter(!is.na(dif)) %>% 
#   filter(name == "area_glacier_km2") %>% 
#   mutate(X = st_coordinates(.)[,1],
#          Y = st_coordinates(.)[,2]) %>% 
#   st_drop_geometry() %>% 
#   
#   ggplot() + 
#   geom_hex(aes(X, Y, ))
  
  
new_all_ter_dem <- new_all_ter %>% 
  st_drop_geometry() %>% 
  group_by(year, type_long) %>% 
  summarise(#dem_avg_05 = quantile(dem_avg, probs = 0.25, na.rm = T),
            dem_avg_50 = quantile(dem_avg, probs = 0.50, na.rm = T))
            #dem_avg_95 = quantile(dem_avg, probs = 0.75, na.rm = T))


dem2010 <- new_all_ter_dem %>% 
  filter(year <= 2010) %>% 
  group_by(type_long) %>% 
  do(tidy = tidy(lm(dem_avg_50 ~ year, data = .))) %>% 
  unnest(tidy) %>% 
  filter(term == "year")
dem2019 <- new_all_ter_dem %>% 
  filter(year > 2010) %>% 
  group_by(type_long) %>% 
  do(tidy = tidy(lm(dem_avg_50 ~ year, data = .))) %>% 
  unnest(tidy) %>% 
  filter(term == "year")

(my_dem <- new_all_ter_dem %>% 
  ggplot() + 
  facet_wrap(~type_long, scales = "free")+
  geom_point(aes(year, dem_avg_50)) +
  geom_smooth(data = new_all_ter_dem %>% filter(year <= 2010), aes(year, dem_avg_50), method = "lm", se = F, color = "blue") +
  geom_smooth(data = new_all_ter_dem %>% filter(year > 2010), aes(year, dem_avg_50), method = "lm", se = F, color = "red") +
  geom_text(data = dem2010, aes(1990, c(1940, 1720, 1830), label = paste("Slope =",signif(estimate, 2),"\n", #c(1960, 1900, 1760, 1700, 1860, 1820)
                                                                        "p =",signif(p.value, 2),"\n",
                                                                        "Std. error =",signif(std.error, 2))), color = "blue", size = 3) +
  geom_text(data = dem2019, aes(2006, c(1960, 1730, 1830), label = paste("Slope =",signif(estimate, 2),"\n", #c(1980, 1910, 1765, 1710, 1880, 1830)
                                                                          "p =",signif(p.value, 2),"\n",
                                                                          "Std. error =",signif(std.error, 2))), color = "red", size = 3) +
  
  scale_shape_manual(values = c(21,22)) +
  scale_color_manual(values = c("red","blue","black")) +
  egg::theme_article() + 
  theme(aspect.ratio = 1) +
  labs(y = "Median glacier elevation (m a.s.l.)", x = "Year"))


ggsave(filename = "Code/3_analysis/!Figures_Final/Figure_10.tif", device = "tiff", width = 10, height = 10,  dpi = 300)
knitr::plot_crop("Code/3_analysis/!Figures_Final/Figure_10.tif")


new_all_ter_dem %>% 
  filter(year == 2020) %>% 
  ungroup() %>% 
  dplyr::select(type_long, dem_avg_50)



new_all_ter %>% 
  filter(year == 2020) %>% 
  ggplot() + 
  geom_sf(aes(color = dem_avg))
  


# #### PLOT SLOPE #### 
# 
# new_all_ter_slp <- new_all_ter %>% 
#   group_by(year, type_long) %>% 
#   summarise(slp_avg_05 = quantile(slp_avg, probs = 0.25, na.rm = T),
#             slp_avg_50 = quantile(slp_avg, probs = 0.50, na.rm = T),
#             slp_avg_95 = quantile(slp_avg, probs = 0.75, na.rm = T))
# 
# 
# slp2010 <- new_all_ter_slp %>% 
#   filter(year <= 2010) %>% 
#   group_by(type_long) %>% 
#   do(tidy = tidy(lm(slp_avg_50 ~ year, data = .))) %>% 
#   unnest(tidy) %>% 
#   filter(term == "year")
# slp2019 <- new_all_ter_slp %>% 
#   filter(year >= 2010) %>% 
#   group_by(type_long) %>% 
#   do(tidy = tidy(lm(slp_avg_50 ~ year, data = .))) %>% 
#   unnest(tidy) %>% 
#   filter(term == "year")
# 
# my_slp <- new_all_ter_slp %>% 
#   ggplot() + 
#   facet_wrap(~type_long, scales = "free")+
#   geom_point(aes(year, slp_avg_50)) +
#   geom_smooth(data = new_all_ter_slp %>% filter(year <= 2010), aes(year, slp_avg_50), method = "lm", se = F, color = "red") +
#   geom_smooth(data = new_all_ter_slp %>% filter(year >= 2010), aes(year, slp_avg_50), method = "lm", se = F, color = "blue") +
#   geom_text(data = slp2010, aes(1990, c(22.5, 22.5, 17.8), label = paste("Slope =",signif(estimate, 2),"\n", #c(1960, 1900, 1760, 1700, 1860, 1820)
#                                                                          "p =",signif(p.value, 2),"\n",
#                                                                          "Std. error =",signif(std.error, 2))), color = "red", size = 2) +
#   geom_text(data = slp2019, aes(c(2006, 2012, 2012), c(22.3, 21.5, 17.3), label = paste("Slope =",signif(estimate, 2),"\n", #c(1980, 1910, 1765, 1710, 1880, 1830)
#                                                                          "p =",signif(p.value, 2),"\n",
#                                                                          "Std. error =",signif(std.error, 2))), color = "blue", size = 2) +
#   scale_shape_manual(values = c(21,22)) +
#   scale_color_manual(values = c("red","blue","black")) +
#   egg::theme_article() + 
#   theme(aspect.ratio = 1) +
#   labs(y = "Median Glacier Slope (degrees)", x = "Year")


#### PLOT ASPECT ####

# new_all_ter_asp <- new_all_ter %>%
#   st_drop_geometry() %>% 
#   mutate(asp  = as.numeric(as.character(cut(asp_avg, seq(-180,180,20), seq(-175,175,20))))) %>%
#   filter(!is.na(asp)) %>% 
#   group_by(year, type_long, asp) %>% 
#   summarize(n = n(),
#             area = sum(area_glacier_km2, na.rm=T))
# 
# new_all_ter_asp %>%
#   # ungroup() %>% 
#   # group_by(asp_avg_group, year, type_long) %>%
#   # summarize(mean = mean(area)) %>% 
#   group_by(asp, type_long) %>% 
#   # filter(year >2010) %>% 
#   do(tidy = tidy(lm(area ~ year, data = .))) %>% 
#   unnest(tidy) %>% 
#   filter(term == "year") %>% 
#   ggplot() + 
#     geom_point(aes(asp, estimate, fill = estimate), size = 3, shape = 21) +
#     geom_hline(yintercept = 0, linetype = 1) +
#     geom_text(aes(x = 0, y = -0.85, label = "-1")) +
#     # geom_text(aes(x = 0, y = 0.15, label = "0")) +
#     geom_text(aes(x = 0, y = 1.15, label = "1")) +
#     scale_fill_gradientn(colours = RColorBrewer::brewer.pal(10, name = "RdYlBu"), limits = c(-2,2)) +
#     guides(fill = guide_colorsteps(barheight = 8, frame.colour = "black")) +
#     # scale_x_continuous(breaks = seq(-180+22.5,180,45), labels = toupper(c("ssw","wsw","wnw","nnw","nne","ene","ese","sse"))) +
#     coord_polar(theta = "x", start = pi, direction = 1, clip = "on") +
#     egg::theme_article() + 
#     facet_wrap(~type_long) +
#     theme(panel.grid.major = element_line(color = "grey90", linetype = 2),
#           axis.text.y = element_blank(),
#           axis.ticks.y = element_blank(),
#           panel.background = element_blank(),
#           panel.border = element_blank()) +
#     labs(x = "", y = "", fill = "Glacier Trend (n a<sup>-1</sup>) <br> 1984-2019") +
#     theme(legend.title = element_markdown())
#     
# )
# 
# new_all_ter %>% 
#   mutate(asp_avg_group  = cut(asp_avg, seq(-180,180,10), seq(-175,175,10))) %>%
#   filter(!is.na(asp_avg_group)) %>% 
#   group_by(year, asp_avg_group, type_long) %>% 
#   summarise(n = n(), 
#             sum = sum(area_km2, na.rm = T)) %>% 
#   ungroup() %>% 
#   group_by(asp_avg_group, type_long) %>% 
#   filter(type_long == "Clean ice") %>% 
#   do(tidy = tidy(lm(n ~ year, data = .))) %>% 
#   unnest(tidy) %>% 
#   filter(term == "year") %>% 
#   mutate(asp_avg_group = as.numeric(as.character(asp_avg_group))) %>% 
#   arrange(-estimate)


# my_dem#/my_asp






#### MEDIAN GLACIE MAP ####

temp2020 <- new_all_ter %>%
  filter(year == 2020) %>% 
  st_centroid() 
st_crs(temp2020) <- 3005

temp2020 %>% st_write("Data/2020_glacier_elevation.gpkg")
  ggplot() + 
  geom_sf(data = bc_neighbours()) +
  geom_sf(data = temp2020 %>% arrange(-dem_avg), 
          aes(color = dem_avg)) + 
    scale_color_distiller(palette = "Spectral", direction = 1) + 
    scale_size_continuous(range = c(1,1)) + 
    geom_sf(data = regions, fill = NA, size = 2, color = "black") +
    guides(color = guide_colorsteps(frame.colour = "black", 
                                   barwidth = 15, 
                                   barheight = 0.6, 
                                   ticks.colour = "black",
                                   title.position="top", 
                                   title.hjust = 0.5)) +
    coord_sf(crs = st_crs(3005), expand = F) +
    labs(color = "Median glacier elevation (m a.s.l.)") +
    theme(legend.position = "bottom", 
          legend.direction = "horizontal")

  ggsave(filename = "Code/3_analysis/Fig 13 Elevation Time/median_z_map.tif", device = "tiff", width = 8, height = 12,  dpi = 300)
  knitr::plot_crop("Code/3_analysis/Fig 13 Elevation Time/median_z_map.tif")
  
  
# NEW ASPECT PLOT FOR REVIEW 
  new_all_ter %>%
    st_drop_geometry() %>%
    filter(year %in% c(1984,2020)) %>%
    group_by(region, year) %>%
    summarize(area = sum(area_glacier_km2, na.rm = T),
              dem = mean(dem_avg, na.rm = T),
              asp = mean(circular::circular(asp_avg, units = "degrees"), na.rm = T)) %>%
    ggplot() +
    geom_point(aes(asp, dem, color = region, shape = as.factor(year))) +
    ggrepel::geom_text_repel(data = . %>% filter(year == 2020), aes(asp, dem, label = region, color = region), show.legend = F) +
    scale_shape_manual(values = 1:11) +
    theme_bw() +
    theme(aspect.ratio = 1) +
    coord_polar() +
    scale_x_continuous(limits = c(0,360), breaks = seq(0,360, 30)) +
    labs(x = "Apect (°)", y = "Elevation (m a.s.l.)", color = "Region", shape = "Year")

  ggsave(filename = "Code/3_analysis/Fig 13 Elevation Time/median_asp_polar.tif", device = "tiff", width = 8, height = 12,  dpi = 300)
  knitr::plot_crop("Code/3_analysis/Fig 13 Elevation Time/median_asp_polar.tif")
  

# NEW PLOT MIDPOINT ELEVATION
  
  new_all_ter %>%
    st_drop_geometry() %>%
    dplyr:: select(region, year, dem_avg, dem_min) %>% 
    group_by(region, year) %>% 
    summarise(dem_avg = mean(dem_avg, na.rm=T),
              dem_min = mean(dem_min, na.rm=T)) %>% 
    ggplot() + 
    geom_point(aes(dem_avg, dem_min, color = year)) + 
    ggrepel::geom_text_repel(data = . %>% group_by(region) %>% 
                               summarize(dem_avg = max(dem_avg), dem_min = max(dem_min)), 
                             aes(dem_avg, dem_min,label = region)) +
    guides(color = guide_colorbar(frame.colour = "black", 
                                    barwidth = 15, 
                                    barheight = 0.6, 
                                    ticks.colour = "black",
                                    title.position="top", 
                                    title.hjust = 0.5)) +
    egg::theme_article() + 
    theme(aspect.ratio = 1, 
          legend.position = "bottom", 
          legend.direction = "horizontal") + 
    labs(x = "Median glacier elevation (m a.s.l.)", 
         y = "Minimum glacier elevation (m a.s.l.)", 
         color = "Year")

  ggsave(filename = "Code/3_analysis/Fig 13 Elexvation Time/dem_min_mean.tif", device = "tiff", width = 8, height = 12,  dpi = 300)
  knitr::plot_crop("Code/3_analysis/Fig 13 Elevation Time/dem_min_mean.tif")
  
  
# SCATTER PLOT 
  
pp <-   new_all_ter %>%
    st_drop_geometry() %>%
    filter(year %in% c(1984,2020)) %>% 
    ggplot() + 
    geom_point(aes(area_glacier_km2, dem_min, color = as.factor(year)), size = 0.3, alpha = 0.1) +
    geom_smooth(aes(area_glacier_km2, dem_min, color = as.factor(year))) + 
    scale_x_log10() + 
    egg::theme_article() + 
    theme(aspect.ratio = 1) + 
    scale_color_manual(values = c("red","blue")) + 
    labs(x = expression(Glacier~area~{km^2}), 
         y = "Minimum glacier elevation (m a.s.l.)", color = "Year") | 
  new_all_ter %>%
    st_drop_geometry() %>%
    filter(year %in% c(1984,2020)) %>% 
    ggplot() + 
    geom_point(aes(area_glacier_km2, dem_max, color = as.factor(year)), size = 0.3, alpha = 0.1) +
    geom_smooth(aes(area_glacier_km2, dem_max, color = as.factor(year))) + 
    scale_x_log10() + 
    egg::theme_article() + 
    theme(aspect.ratio = 1) + 
    scale_color_manual(values = c("red","blue")) + 
    labs(x = expression(Glacier~area~{km^2}), 
         y = "Maximum glacier elevation (m a.s.l.)", color = "Year") 

  ggsave(plot = pp, filename = "Code/3_analysis/Fig 13 Elevation Time/dem_min_mean_area.tif", device = "tiff", width = 10, height = 12,  dpi = 300)
  knitr::plot_crop("Code/3_analysis/Fig 13 Elevation Time/dem_min_mean_area.tif")
  
  
  
  
#   # |
# ggplot() + 
#   geom_sf(data = bc_neighbours()) +
#   geom_sf(data = temp2020, aes(fill = area_glacier_km2), shape = 21, size = 1) + 
#   scale_fill_distiller(palette = "YlOrRd") + 
#   geom_sf(data = regions, fill = NA, size = 2, color = "black") +
#   theme_void()) /
# ggplot() + 
#   geom_sf(data = bc_neighbours()) +
#   geom_sf(data = temp2020, aes(fill = nparts), shape = 21, size = 1) + 
#   scale_fill_distiller(palette = "YlOrRd", direction = 1) + 
#   geom_sf(data = regions, fill = NA, size = 2, color = "black") +
#   theme_void() 
# 
# 
# 
# read_stars("")
# 
# RColorBrewer::display.brewer.all()
# 
# 
# z`new_all_ter %>%
#   st_drop_geometry() %>% 
#   mutate(year_group = case_when(year <= 2010 ~ "1984-2010", 
#                                 year >2010 ~ "2011-2020")) %>%
#   dplyr::select(year_group, n, region, type_short, area_glacier_km2, dem_avg, year) %>% 
#   group_by(year_group, region, type_short) %>% 
#   summarise(dem_avg = mean(dem_avg, na.rm = T), 
#             area_glacier_km2 = mean(area_glacier_km2, na.rm = T)) %>% 
#   pivot_wider(id_cols = c(region, type_short), 
#               names_from = year_group, 
#               values_from = c(dem_avg, area_glacier_km2))
# ) %>% 
#   mutate(dem_avg = `dem_avg_2011-2020`-`dem_avg_1984-2010`,
#          area = `area_glacier_km2_2011-2020`-`area_glacier_km2_1984-2010`)
# 
# 
# 
# 
# 
# s1984 <- new_all_ter %>%
#   filter(year == 1984) %>% 
#   # dplyr::select(n) %>% 
#   st_centroid() 
# s2020 <- new_all_ter %>%
#   filter(year == 2020) %>% 
#   # dplyr::select(n) %>% 
#   st_centroid() 
# sDif <- full_join(s1984,
#           new_all_ter %>%
#             st_drop_geometry() %>% 
#             mutate(year_group = case_when(year <= 2010 ~ "1984-2010", 
#                                           year >2010 ~ "2011-2020")) %>%
#             dplyr::select(year_group, n, region, type_short, area_glacier_km2, dem_avg, year) %>% 
#             group_by(year_group, n, region, type_short) %>% 
#             summarise(dem_avg = mean(dem_avg, na.rm = T), 
#                       area_glacier_km2 = mean(area_glacier_km2, na.rm = T)) %>% 
#               pivot_wider(id_cols = c(n, region, type_short), 
#                           names_from = year_group, 
#                           values_from = c(dem_avg, area_glacier_km2))) %>% 
#     mutate(dem_avg = `dem_avg_2011-2020`-`dem_avg_1984-2010`,
#            area = `area_glacier_km2_2011-2020`-`area_glacier_km2_1984-2010`)
# 
# sDif %>% 
#   ggplot() + 
#   geom_hist(aes(color = area))
# 
# 
# pivot_longer(cols = c("nparts", "area_glacier_km2", "area_water_km2", "dem_avg", "asp_avg")) %>% 
#   group_by(year_group, n, name) %>% 
#   filter(!is.na(value)) %>% 
#   do(tidy = tidy(lm(value ~ year, data = .)))%>%
#   unnest(tidy) %>% 
#   filter(term == "year")