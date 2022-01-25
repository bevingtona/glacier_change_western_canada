library(patchwork)
library(exactextractr)
library(stars)
library(broom)
library(sf)
library(tidyverse)
library(ecmwfr)

gl <- read_sf("Data/glaciers14411.sqlite") #gl_1985_2005.gpkg")

regions <- read_sf("../../2018_RGI2/manuscript_outlines/Bolch/glacinvent_regions/bca_glacier_regions.shp") %>% 
  rename(region = Constrain)

#### AUTHENTICATE #### 

# wf_set_key(user = "71149",
#            key = "76f3cbc5-bb4f-4347-8a51-80dc2bf1ca66",
#            service = "cds")

#### REQUEST DATA #### 

# Set years
years <- as.character(1984:2020)

# Set months
months <- c("01","02","03","04","05","06","07","08","09","10","11","12")

# Set variables 
var <- c('2m_temperature') 

#### SET NAMES PER VARIABLE ####

  # Print Name
  print(var)
  
  # Make band names for output 
  bands <-
    do.call(c, lapply(years, function(y){
      out <- c()
      for(m in months){
        for(v in var) { 
          out <- c(out,paste0(y,"-",m,"-01__",v))
        }}
      return(out)}))
  
  # Out name 
  target <- paste0("Data/era/BC-ERA5-land_",
                   min(years),"_",
                   max(years),"_",
                   min(months),"_",
                   max(months),"mm_",
                   length(var),"_",
                   var,".grib")
  
#### READ DATA #### 

# READ STARS
my_temp <- stars::read_stars(target)

# SET PROJECTION
st_crs(my_temp) <- 4326

# SET BAND NAMES 
my_temp <- my_temp %>% stars::st_set_dimensions(which = 3, values = bands)

# CLIP TO BBOX OF REGIONS
my_temp <- my_temp[regions %>% st_buffer(100000) %>% st_transform(4326) %>% st_bbox() %>% st_as_sfc()]

#### PREP TIMESERIES DATA ####

# ERA STACK TO POLYGON
ts <- my_temp %>% st_as_stars() %>% st_as_sf()

# ADD UID
ts <- ts %>% mutate(id = row_number())
s <- ts %>% dplyr::select(id, geometry)

# FILTER BOUNDS
ts <- ts %>% st_intersection(regions %>% st_transform(st_crs(ts)) %>% st_bbox() %>% st_as_sfc())

# ADD  REGIONS
ts <- ts %>% st_join(regions %>% st_transform(st_crs(ts)) %>% dplyr::select(region))

# FILTER GLACIERS
# ts <- ts %>% st_intersection(new_all_ct_2020_moving %>% st_transform(st_crs(ts)) %>% st_geometry()) 

# PIVOT and METRE TO C
ts <- ts %>% 
  st_drop_geometry() %>% 
  pivot_longer(c(-id, -region), names_to = "date", values_to = "val") %>% 
  mutate(date = as.Date(date), 
         val = val-273.15)

# DATE FORMAT AND MONTH FILTER
ts_f <- ts %>%
    mutate(year = lubridate::year(date),
             hydroyear = lead(year, 4),
           month.num = lubridate::month(date),
         month = paste(format(date, "%m"),format(date, "%b")),
         season = case_when(month %in% c("12 Dec","01 Jan","02 Feb") ~ "Winter",
                            month %in% c("03 Mar","04 Apr","05 May") ~ "Spring",
                            month %in% c("06 Jun","07 Jul","08 Aug") ~ "Summer",
                            month %in% c("09 Sep","10 Oct","11 Nov") ~ "Fall"))

# ORDER SEASONS 
ts_f$season = factor(ts_f$season, levels=c("Fall","Winter","Spring","Summer"))

# MEAN OF ALL PIXELS PER YEAR  
# ts_f_g <- ts_f %>%
#   group_by(year, season) %>% 
#   summarise(mean = mean(val),
#             min = min(val),
#             max = max(val))

#### MAP ANNOMALIES ####
  
  # ERA Pixel linear models (3 time periods)
  ts_annom <- ts_f %>% 
    group_by(id, season, region) %>% 
    summarise(val = mean(val))
  ts_annom_2010 <- ts_f %>% 
    filter(year <=2010) %>% 
    group_by(id, season, region) %>% 
    summarise(val = mean(val)) 
  ts_annom_2020 <- ts_f %>% 
    filter(year >2010) %>%
    group_by(id, season, region) %>% 
    summarise(val = mean(val)) 
  
  # JOING LM TO GEOMETRY
  ts_annom_j <- full_join(s %>% select(id, geometry), ts_annom)
  ts_annom_j_2010 <- full_join(s %>% select(id, geometry), ts_annom_2010)
  ts_annom_j_2020 <- full_join(s %>% select(id, geometry), ts_annom_2020)
  
  ts_annom_j_2010 <- ts_annom_j_2010 %>% rename(mean2010 = val)#, ts_annom_j %>% st_drop_geometry() %>% rename(mean = val)) %>% mutate(annom = mean2010 - mean)
  ts_annom_j_2020 <- ts_annom_j_2020 %>% rename(mean2020 = val)#, ts_annom_j %>% st_drop_geometry() %>% rename(mean = val)) %>% mutate(annom = mean2020 - mean)
  
# MAP FUNCTION
  j <- full_join(ts_annom_j_2010 %>% select(id, season, region, mean2010) %>% st_drop_geometry(),
            ts_annom_j_2020 %>% select(id, season, region, mean2020))
  
  j <- j %>% mutate(dif = mean2020-mean2010) %>% st_sf() %>% 
    filter(!is.na(dif))
  
  j %>%
    ggplot() + 
      geom_sf(aes(fill = dif), color = NA) +
      # geom_sf(data = bc_neighbours(), fill = NA, color = "grey") +
      geom_sf(data = regions, fill = NA, color = "black") +
      scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(10,"RdYlBu")), 
                           values = scales::rescale(c(-2,0,2)),
                           guide = "colorbar",
                           limits=c(-2,2),
                           breaks = seq(-2,2,0.5)
                           ) +
      guides(fill = guide_colorsteps(frame.colour = "black", 
                                     barwidth = 15, 
                                     barheight = 0.6, 
                                     ticks.colour = "black",
                                     title.position = "top", 
                                     title.hjust = 0.5)) +
      coord_sf(crs = st_crs(3005), expand = F) +
      facet_wrap(~season, ncol = 1) +
      egg::theme_article() +
      labs(fill = "Mean monthly air temperature (°C) \n Composite anomaly", title = "A") +
      theme(legend.position = "bottom")
  
  filename <- paste0("Code/3_analysis/Fig 14 ERA/", "Fig_14_era_T.tif")
  ggsave(filename = filename, device = "tiff", height = 10,  dpi = 300)
  knitr::plot_crop(filename)
    
#### REGIONAL REGRESSION ####

library(DBI)
library(RSQLite)
  
lyr <- paste0("Data/new_all_join_id_filter_20210625-0942.gpkg")
mydb <- dbConnect(RSQLite::SQLite(), lyr)
  
new_all_join_id_filter <- 
  dbGetQuery(mydb, "SELECT year, region, area_glacier_km2, geom FROM \"new_all_join_id_filter_20210625-0942\"") %>%
  sf::st_as_sf()
  
reg_temp <- full_join(
  new_all_join_id_filter %>% 
    st_drop_geometry() %>% 
    group_by(region, year) %>% 
    summarise(glacierSum = sum(area_glacier_km2, na.rm = T)),
  ts_f %>% 
    filter(!is.na(region)) %>% 
    select(region, hydroyear, season, val) %>%
    rename(year = hydroyear) %>% 
    group_by(region, year, season) %>%
    summarize(tempMean = mean(val, na.rm = T)))

  # reg_temp %>% 
  #   group_by(year, region, glacierSum) 
    
  # reg_temp %>% 
  #   filter(year %in% c(2010,2020)) %>% 
  #   group_by(year, region, glacierSum, season) %>% 
  #   summarise(tempMean = mean(tempMean, na.rm = T)) %>% 
  #   pivot_wider(names_from = year, values_from = c(glacierSum, tempMean)) %>% 
  #   mutate(glacierSum = glacierSum_2020-glacierSum_2010,
  #          tempMean = tempMean_2020-tempMean_2010) %>% 
  #   ggplot(aes(tempMean, glacierSum)) + 
  #   geom_point() + 
  #   geom_smooth(method = "lm", se = F)   + 
  #   # scale_y_log10() +
  #   geom_label(aes(label = region)) +
  #   facet_wrap(~season, ncol = 2, scales = "free") +
  #   egg::theme_article() + 
  #   theme(aspect.ratio = 1) +
  #   labs(x = "Temperature change (Δ°C)", 
  #        y = expression(Glacier~change~(km^2)), 
  #        title = "2020-2010")
  
reg_temp_lm <- reg_temp %>% 
  group_by(region, season) %>% 
  do(tidy = tidy(lm(glacierSum ~ tempMean, data = .))) %>% 
  unnest(tidy) %>% 
  filter(term == "tempMean") %>% 
  mutate(p.value = case_when(p.value < 0.01 ~ "<0.01",
                                   p.value < 0.05 ~ "<0.05",
                                   TRUE ~ ">=0.05"))

FigA <- ggplot(reg_temp_lm) + 
  geom_hline(yintercept = 0, color = "grey60", linetype = 2) + 
  geom_errorbar(aes(region, ymin = estimate-std.error, ymax = estimate+std.error, color = p.value), width = 0.2) + 
  geom_point(aes(region, estimate, shape = p.value, fill = p.value), size = 3) + 
  # ggrepel::geom_text_repel(aes(region, estimate, label = paste(round(estimate,0),"±",signif(std.error,1))), size = 3) + 
  scale_shape_manual(values = c(21,24,22)) +
  scale_fill_manual(values = c("black","grey50","grey90")) +
  scale_color_manual(values = c("black","grey50","grey80")) +
  facet_wrap(~season, scales = "free", ncol = 4) +
  egg::theme_article() + 
  theme(aspect.ratio = 1) + 
  coord_flip() + 
  labs(y = expression(Regression~coefficient~km^{"2"}~{"°C"}^{"-1"}), x = "", title = "A") 

# filename <- "Code/3_analysis/!Figures_Final/Figure_12A.tif"
# ggsave(filename = filename, device = "tiff", height = 8, width = 13, dpi = 300)
# knitr::plot_crop(filename)


# reg_temp %>% 
#   filter(year %in% c(1984,2020)) %>% 
#   pivot_wider(id_cols = c(region, season), names_from = year, values_from = c(glacierSum, tempMean)) %>% 
#   mutate(glacierChange = glacierSum_2020 - glacierSum_1984,
#          tempMean = tempMean_2020 - tempMean_1984) %>% 
#   ggplot() + 
#   geom_point(aes(tempMean, glacierChange)) + 
#   geom_smooth(aes(tempMean, glacierChange), method = "lm") + 
#   ggrepel::geom_text_repel(aes(tempMean, glacierChange, label = region)) +
#   geom_text(data = reg_temp %>% 
#               filter(year %in% c(1984,2020)) %>%
#               pivot_wider(id_cols = c(region, season), names_from = year, values_from = c(glacierSum, tempMean)) %>%
#               mutate(glacierChange = glacierSum_2020 - glacierSum_1984,
#                      tempMean = tempMean_2020 - tempMean_1984) %>%
#               group_by(season) %>%
#               do(tidy = tidy(lm(glacierChange ~ tempMean, data = .))) %>%
#               unnest(tidy) %>%
#               filter(term == "tempMean"),
#             aes(c(0,-2,-0.2,0.5), c(500,500,500,800),
#                 label = paste("Slope =",signif(estimate, 3),#"\n",
#                               "p =",signif(p.value, 2),#"\n",
#                               "Std. error =",signif(std.error, 2))),
#             color = "blue", size = 3) +
#   facet_wrap(~season, scales = "free", ncol = 4) + 
#   egg::theme_article() +
#   theme(aspect.ratio = 1) + 
#   labs(x = "Mean monthly air temperature change 1984-2020 °C", 
#        y = expression(Glacier~area~change~"1984-2020"~km^{"2"}), 
#        title = "A") 
# 
# filename <- paste0("Code/3_analysis/Fig 14 ERA/", "Fig_14_era_change_T.tif")
# ggsave(filename = filename, device = "tiff", height = 8, width = 13, dpi = 300)
# knitr::plot_crop(filename)
