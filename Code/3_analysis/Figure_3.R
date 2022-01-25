
#### FIGURE SCATTERPLOT COUNT / TYPE PERCENTAGE ####

(scatter <-
  new_all_join_id %>% 
  st_drop_geometry() %>%
  filter(year == 2020,
         method == "moving",
         area_glacier_km2 > 0) %>% 
  group_by(region, type_long) %>% 
  summarise(n = n(), sum = sum(area_glacier_km2, na.rm = T)) %>% 
  ungroup() %>% group_by(region) %>%
  mutate(tot = sum(n), perc = 100*n/tot) %>% 
  ggplot() + 
    geom_point(aes(n, perc, shape = type_long, fill = type_long, color = type_long), size = 2.5, show.legend = T) + 
    geom_text_repel(aes(n, perc, label = region), size = 2) +
    scale_x_log10() +
    scale_y_log10(breaks = c(1,10,100), 
                  labels = paste0(c(1,10,100),"%")) +
  scale_shape_manual(values = c(16,21,3)) +
  scale_color_manual(values = c("black","black","dark green")) +
  scale_fill_manual(values = c("black","red",NA)) +
  labs(shape = "Glacier type",
       color = "Glacier type",
       fill = "Glacier type") +
    egg::theme_article() +
    theme(aspect.ratio = 1, 
          text = element_text(color = "black"), 
          panel.border = element_rect(colour = "black"), 
          axis.text = element_text(face = "plain", colour = "black"),
          axis.ticks = element_line(colour = "black"),
          legend.position = c(0.8,0.2)) + 
    labs(x = "Total number of glaciers (log scale)", y = "Percent of glaciers per region (log scale)", 
         shape = "Glacier type", fill = "Glacier type") + 
    annotation_logticks())

# filename <- paste0("Code/3_analysis/!Figures_Final/","Figure_3.tiff")
# ggsave(plot = scatter, filename = filename, device = "tiff", height = 5, width = 5, dpi = 300)
# knitr::plot_crop(filename)


# MIN / MAX PERCENT per TYPE STATISTICS

for(y in c(1984,2020)){
  
  print(y)
  
  min_max_type_stats <- new_all_join_id %>% 
    st_drop_geometry() %>%
    filter(year == y,
           method == "moving",
           area_glacier_km2 > 0) %>% 
    group_by(region, type_long) %>% 
    summarise(n = n(), sum = sum(area_glacier_km2, na.rm = T)) %>% 
    ungroup() %>% 
    group_by(region) %>%
    mutate(tot = sum(n), perc = 100*n/tot)
  
  print(min_max_type_stats %>% 
    dplyr::select(-region) %>% 
    ungroup() %>% 
    group_by(type_long) %>% 
    summarize(sum_n = sum(n)))
  
  min_max_type_stats %>% write.csv(sub(".tiff",paste0(y,".csv"),filename))}
