library(patchwork)
library(exactextractr)
library(stars)
library(broom)
library(sf)
library(tidyverse)
library(ecmwfr)

# citation("ecmwfr")

# REGIONS ####

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
variables <- c('total_precipitation')

#### DOWNLOAD PER VARIABLE ####

# Make band names for output 
bands <-
  do.call(c, lapply(years, function(y){
    out <- c()
    for(m in months){
      for(v in variables) { 
        out <- c(out,paste0(y,"-",m,"-01__",v))
      }}
    return(out)}))

# Out name 
target <- paste0("Data/era/BC-ERA5-land_",
                 min(years),"_",
                 max(years),"_",
                 min(months),"_",
                 max(months),"mm_",
                 length(variables),"_",
                 variables,".grib")

rm(years, months, variables)

#### READ DATA #### 

# READ STARS
my_precip <- stars::read_stars(target)

# SET PROJECTION
st_crs(my_precip) <- 4326

# SET BAND NAMES 
my_precip <- my_precip %>% stars::st_set_dimensions(which = 3, values = as.Date(bands))

# CLIP TO BBOX OF REGIONS
my_precip <- my_precip[regions %>% st_buffer(100000) %>% st_transform(4326) %>% st_bbox() %>% st_as_sfc()]

#### PREP TIMESERIES DATA ####

# ERA STACK TO POLYGON
ps <- my_precip %>% st_as_stars() %>% st_as_sf()

# ADD UID
ps <- ps %>% mutate(id = row_number())
s <- ps %>% dplyr::select(id, geometry)

# FILTER BOUNDS
ps <- ps %>% st_intersection(regions %>% st_transform(st_crs(ps)) %>% st_bbox() %>% st_as_sfc())

# ADD  REGIONS
ps <- ps %>% st_join(regions %>% st_transform(st_crs(ps)) %>% dplyr::select(region))

# FILTER GLACIERS
# ps_glaciers_only <- ps %>% st_intersection(gl %>% st_centroid() %>% st_transform(st_crs(ps)) %>% st_geometry())

# PIVOT and METRE TO MM
ps_mm <- ps %>% 
  st_drop_geometry() %>%   
  pivot_longer(c(-id, -region), names_to = "date", values_to = "val") %>% 
  mutate(date = as.Date(date),
         val_mm_sum = val*1000*lubridate::days_in_month(lubridate::month(date)))

# DATE FORMAT AND MONTH FILTER
ps_f <- ps_mm %>% mutate(year = lubridate::year(date),
                         hydroyear = lead(year, 4),
                         month.num = lubridate::month(date),
                         month = paste(format(date, "%m"),format(date, "%b")),
                         season = case_when(month %in% c("12 Dec","01 Jan","02 Feb") ~ "Winter",
                                            month %in% c("03 Mar","04 Apr","05 May") ~ "Spring",
                                            month %in% c("06 Jun","07 Jul","08 Aug") ~ "Summer",
                                            month %in% c("09 Sep","10 Oct","11 Nov") ~ "Fall"))

# ORDER SEASONS 
ps_f$season = factor(ps_f$season, levels=c("Fall","Winter","Spring","Summer","All"))

#### #### MAP ANNOMALIES (SEASONAL) ####

# ERA Pixel linear models (3 time periods)
ps_annom <- ps_f %>% 
  group_by(year, id, season, region) %>% 
  summarise(val = mean(val_mm_sum, na.rm = T)) 
  
ps_annom_2010 <- ps_annom %>% 
  filter(year <=2010) %>% 
  group_by(id, season, region) %>% 
  summarise(val = mean(val, na.rm = T))

ps_annom_2020 <- ps_annom %>% 
  filter(year >2010) %>%
  group_by(id, season, region) %>% 
  summarise(val = mean(val, na.rm = T))

# JOING LM TO GEOMETRY
ps_annom_j <- full_join(s %>% dplyr::select(id, geometry), ps_annom)
ps_annom_j_2010 <- full_join(s %>% dplyr::select(id, geometry), ps_annom_2010)
ps_annom_j_2020 <- full_join(s %>% dplyr::select(id, geometry), ps_annom_2020)

ps_annom_j_2010 <- ps_annom_j_2010 %>% rename(mean2010 = val)#full_join(, ps_annom_j %>% st_drop_geometry() %>% rename(mean = val)) %>% mutate(annom = mean2010 - mean)
ps_annom_j_2020 <- ps_annom_j_2020 %>% rename(mean2020 = val)#full_join(, ps_annom_j %>% st_drop_geometry() %>% rename(mean = val)) %>% mutate(annom = mean2020 - mean)

# MAP FUNCTION
j <- full_join(ps_annom_j_2010 %>% dplyr::select(id, season, region, mean2010) %>% st_drop_geometry(),
               ps_annom_j_2020 %>% dplyr::select(id, season, region, mean2020))

j <- j %>% mutate(dif = mean2020-mean2010) %>% st_sf() %>% 
  filter(!is.na(dif))

rm(ps, ps_mm, ps_annom_j, ps_annom_2010, ps_annom_2020, ps_annom_j_2010, ps_annom_j_2020, gl, ps_annom)

j %>% 
  ggplot() + 
  geom_sf(aes(fill = dif), color = NA) +
  geom_sf(data = regions, fill = NA, color = "black") +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9,"BrBG"),
                       values = scales::rescale(c(min(j$dif),0,max(j$dif))), 
                       # limits = c(min(j$dif),max(j$dif)), 
                       # breaks = seq(-50,50,20)
                       ) +
  guides(fill = guide_colorsteps(frame.colour = "black", 
                               barwidth = 15, 
                               barheight = 0.6, 
                               ticks.colour = "black",
                               title.position="top", 
                               title.hjust = 0.5)) +
  coord_sf(crs = st_crs(3005), expand = F) +
  facet_wrap(~season, ncol = 1) +
  egg::theme_article() +
  labs(fill = "Average total monthly precipitation (mm) \nComposite anomaly", title = "B") +
  theme(legend.position = "bottom", 
        legend.direction = "horizontal")

filename <- paste0("Code/3_analysis/Fig 14 ERA/", "Fig_14_era_P.tiff") #format(lubridate::now(), "%Y%m%d-%H%M-"),
ggsave(filename = filename, height = 10,  dpi = 300)
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
  ps_f %>% 
    filter(!is.na(region)) %>% 
    dplyr::select(region, hydroyear, season, val_mm_sum) %>%
    rename(year = hydroyear) %>%
    group_by(region, year, season) %>%
    summarize(tempMean = mean(val_mm_sum, na.rm = T)))

reg_temp_lm <- reg_temp %>%
  group_by(region, season) %>% 
  do(tidy = tidy(lm(glacierSum ~ tempMean, data = .))) %>% 
  unnest(tidy) %>% 
  filter(term == "tempMean") %>% 
  mutate(p.value = case_when(p.value < 0.01 ~ "<0.01",
                             p.value < 0.05 ~ "<0.05",
                             TRUE ~ ">=0.05"))

FigB <- ggplot(reg_temp_lm) + 
  geom_hline(yintercept = 0, color = "grey60", linetype = 2) + 
  geom_errorbar(aes(region, ymin = estimate-std.error, ymax = estimate+std.error, color = p.value), width = 0.2) + 
  geom_point(aes(region, estimate, shape = p.value, fill = p.value), size = 3) + 
  # ggrepel::geom_text_repel(aes(region, estimate, label = paste(signif(estimate,1),"±",signif(std.error,1))), size = 3) + 
  scale_shape_manual(values = c(24,22)) +
  scale_fill_manual(values = c("grey40","grey90")) +
  scale_color_manual(values = c("grey40","grey80")) +
  facet_wrap(~season, scales = "free", ncol = 4) +
  egg::theme_article() + 
  theme(aspect.ratio = 1) + 
  coord_flip() + 
  labs(y = expression(Regression~coefficient~km^{"2"}~{"mm"}^{"-1"}), x = "", title = "B") 


Fig12 <- FigA/FigB

filename <- "Code/3_analysis/!Figures_Final/Figure_12.tif"
ggsave(plot = Fig12, filename = filename, device = "tiff", height = 8, width = 13, dpi = 300)
knitr::plot_crop(filename)

reg_temp %>% 
  filter(year %in% c(1984,2020)) %>% 
  pivot_wider(id_cols = c(region, season), names_from = year, values_from = c(glacierSum, tempMean)) %>% 
  mutate(glacierChange = glacierSum_2020 - glacierSum_1984,
         tempMean = tempMean_2020 - tempMean_1984) %>% 
  ggplot() + 
  geom_point(aes(tempMean, glacierChange)) + 
  geom_smooth(aes(tempMean, glacierChange), method = "lm") + 
  ggrepel::geom_text_repel(aes(tempMean, glacierChange, label = region)) +
  geom_text(data = reg_temp %>% 
              filter(year %in% c(1984,2020)) %>%
              pivot_wider(id_cols = c(region, season), names_from = year, values_from = c(glacierSum, tempMean)) %>%
              mutate(glacierChange = glacierSum_2020 - glacierSum_1984,
                     tempMean = tempMean_2020 - tempMean_1984) %>%
              group_by(season) %>%
              do(tidy = tidy(lm(glacierChange ~ tempMean, data = .))) %>%
              unnest(tidy) %>%
              filter(term == "tempMean"),
            aes(c(-25,-20,-50,40), c(500,500,700,400),
                label = paste("Slope =",signif(estimate, 3),#"\n",
                              "p =",signif(p.value, 2),#"\n",
                              "Std. error =",signif(std.error, 2))),
            color = "blue", size = 3) +
  facet_wrap(~season, scales = "free", ncol = 4) + 
  egg::theme_article() +
  theme(aspect.ratio = 1) + 
  labs(x = "Average total monthly precipitation change 1984-2020 mm", 
       y = expression(Glacier~area~change~"1984-2020"~km^{"2"}), 
       title = "B") 

filename <- paste0("Code/3_analysis/Fig 14 ERA/", "Fig_14_era_change_P.tif")
ggsave(filename = filename, device = "tiff", height = 8, width = 13, dpi = 300)
knitr::plot_crop(filename)
