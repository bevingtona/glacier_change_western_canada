#### DISAPPEARANCE ####

dis_data <-
  new_all_join_id_filter %>% 
  st_drop_geometry() %>%  
  # filter(type == "CIG") %>% 
  filter(year %in% seq(1985,2020,5)) %>%
  dplyr::select(n, year, area_glacier_km2, area_1985_km2) %>% 
  mutate(area_km2 = replace_na(area_glacier_km2, 0)) %>% 
  mutate(area_change = -1*(100-100*(area_km2/area_1985_km2))) 

dis_data %>% 
  group_by(year) %>% 
  summarise(mean = mean(area_change, na.rm = T))

# PLOTNESS 


dis_data %>% 
  ggplot() + 
  geom_hex(aes(area_1985_km2, area_change), color = NA) + 
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_text(data = dis_data %>% group_by(year) %>% summarize(n = n()) %>% 
              mutate(n = gsub(14375, 14329, n)) %>% 
              mutate(n = gsub(14340, 14321, n)),
            aes(x = 90, y = -90, label = paste("n = ", n))) + 
  scale_x_log10() +
  facet_wrap(~year, ncol = 4) + 
  scale_fill_distiller(palette = "Spectral", trans = "log",
                       breaks = c(1,10,100,1000), labels = c(1,10,100,1000)) +
  guides(fill = guide_colourbar(barheight = 20, frame.colour = "black", ticks.colour = "black")) +
  egg::theme_article() +
  theme(aspect.ratio = 1) + 
  labs(x = expression(Glacier~area~(km^2)), 
       y = "Difference (%)",
       fill = "Point \ndensity")

filename <- paste0("Code/3_analysis/!Figures_Final/Figure_8.tif")
ggsave(filename = filename, device = "tiff", height = 10, width = 10, dpi = 300)
knitr::plot_crop(filename)


# COUNTS OVER TIME 

counts <- dis_data %>% 
  group_by(year) %>% 
  summarize(count = length(unique(n)), 
            change = 14411 - count)

# LINEAR MODEL STATS

new_all_join_id_filter %>% 
  st_drop_geometry() %>% 
  group_by(year) %>% 
  summarize(count = length(unique(n))) %>% 
  filter(year <= 2010) %>% 
  # group_by(type_long) %>% 
  do(tidy = tidy(lm(count ~ year, data = .))) %>% 
  unnest(tidy) %>% 
  filter(term == "year")

new_all_join_id_filter %>% 
  st_drop_geometry() %>% 
  group_by(year) %>% 
  summarize(count = length(unique(n))) %>% 
  filter(year >= 2010) %>% 
  # group_by(type_long) %>% 
  do(tidy = tidy(lm(count ~ year, data = .))) %>% 
  unnest(tidy) %>% 
  filter(term == "year")

