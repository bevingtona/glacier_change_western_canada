library(ggpubr)
library(patchwork)

#### RMSE #### 

rmse <- yardstick::rmse(new_all_join_id %>%
                  st_drop_geometry() %>% 
                  filter(year %in% c(2005)) %>% 
                  group_by(method, type_short, type_long, area_1985_km2_group), 
                truth = area_2005_km2, estimate = area_glacier_km2) 

rmse %>% 
  group_by(method) %>% 
  summarize(mean = mean(.estimate, na.rm = T))
rmse %>% 
  group_by(method, type_short, area_1985_km2_group) %>% 
  summarize(mean = mean(.estimate, na.rm = T))

####  SCATTERPLOT 1:1 ####

new_all_join_id %>% 
  st_drop_geometry() %>% 
  filter(year %in% 2005, area_1985_km2<1) %>% 
  ggplot(aes(area_2005_km2, area_glacier_km2)) +
  geom_hex(color = NA, show.legend = T) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 9,  name = "Blues")[3:9], 
                       guide = guide_colorsteps(barheight = 1, barwidth = 10, 
                                                title.position="top", 
                                                title.hjust = 0.5)) +
  facet_wrap(method~paste0(type_long), ncol = 2) +
  geom_smooth(method = "lm", color = "black", se = F, size = 0.7, show_legend = F, linetype=2) +
  geom_text(data = rmse %>% filter(area_1985_km2_group == "<1 sq. km"), 
            aes(0.8,0.1, label = paste("RMSE",signif(.estimate,2))), size = 3) +
  ylim(c(0.05,1)) +
  xlim(c(0.05,1)) +
  stat_cor(label.y.npc = 1, size = 3) +
  stat_regline_equation(label.y.npc = 0.9, size = 3) +
  egg::theme_article() +
  theme(aspect.ratio = 1, plot.title = element_text(hjust = 0.5), 
        legend.position = "bottom") + 
  labs(title = expression(Smaller~than~1~km^2),
       x = expression(Reference~glacier~area~(km^2)),
       y = expression(Automated~glacier~area~(km^2)),
       fill = "Number of glaciers")|

  new_all_join_id %>% 
  st_drop_geometry() %>% 
  filter(year %in% 2005, area_1985_km2>1) %>% 
  ggplot(aes(area_2005_km2, area_glacier_km2)) +
  geom_hex(color = NA, show.legend = T) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 9,  name = "Blues")[3:9], 
                       guide = guide_colorsteps(barheight = 1, barwidth = 10, 
                                                title.position="top", 
                                                title.hjust = 0.5)) +
  facet_wrap(method~paste0(type_long), ncol = 3) +
  geom_smooth(method = "lm", color = "black", se = F, size = 0.7, show_legend = F, linetype=2) +
  geom_text(data = rmse %>% filter(area_1985_km2_group == ">= 1 sq. km"), 
            aes(400,10, label = paste("RMSE",signif(.estimate,2))), size = 3) +
  stat_cor(label.y.npc = 1, size = 3) +
  stat_regline_equation(label.y.npc = 0.9, size = 3) +
  egg::theme_article() +
  theme(aspect.ratio = 1, 
        plot.title = element_text(hjust = 0.5), 
        legend.position = "bottom") + 
  labs(title = expression(Larger~than~1~km^2),
       x = expression(Reference~glacier~area~(km^2)),
       y = expression(Automated~glacier~area~(km^2)),
       fill = "Number of glaciers")

# ggsave("Code/3_analysis/!Figures_Final/Figure_5.tif", device = "tiff", height = 8, width = 11, dpi = 300)
# knitr::plot_crop("Code/3_analysis/!Figures_Final/Figure_5.tif")

#### ADD ERRORS TO DATA ####

my_rmse <- rmse %>% 
  mutate(keep = 
           case_when(type_short == "CIG" & method == "moving" & area_1985_km2_group == "<1 sq. km" ~ "YES", 
                     type_short == "CIG" & method == "moving" & area_1985_km2_group == ">= 1 sq. km" ~ "YES",
                     type_short == "DCG" & method == "moving" & area_1985_km2_group == ">= 1 sq. km" ~ "YES",
                     type_short == "PLG" & method == "moving" & area_1985_km2_group == "<1 sq. km" ~ "YES", 
                     type_short == "PLG" & method == "moving" & area_1985_km2_group == ">= 1 sq. km" ~ "YES", 
                     TRUE ~ "NO")) %>% 
  filter(keep == "YES")


new_all_join_id_filter <- full_join(new_all_join_id,
                                    my_rmse %>% dplyr::select(-.metric,-.estimator)) %>% 
                                    filter(keep == "YES")

new_all_join_id_filter <- new_all_join_id_filter %>% 
  mutate(area_glacier_km2_1sigL = area_glacier_km2 - .estimate,
         area_glacier_km2_1sigH = area_glacier_km2 + .estimate)

rm(new_all_join_id)
filename <- paste0("Data_2021/new_all_join_id_filter_", format(lubridate::now(), "%Y%m%d-%H%M"),".gpkg")
new_all_join_id_filter %>% st_set_crs(3005) %>% st_write(filename)



# rmse %>% 
#   ggplot() + 
#   geom_point(aes(method, .estimate, shape = type_long, fill = type_long), size = 2) + 
#   ggrepel::geom_text_repel(aes(method, .estimate, label = signif(.estimate,2)), size = 3) + 
#   scale_shape_manual(values = c(21,22,24)) +
#   scale_fill_manual(values = c("black", "grey50", "white")) +
#   facet_wrap(~area_1985_km2_group, scales = "free") + 
#   labs(x = "Method", y = "RMSE", shape = "Type", fill = "Type") + 
#   egg::theme_article() + 
#   theme(aspect.ratio = 1)
# 
# filename <- paste0("Code/3_analysis/Fig 5 Manual Compare/", format(lubridate::now(), "%Y%m%d-%H%M-"),"rmse.png")
# ggsave(filename = filename, device = "png", height = 6, dpi = 300)
# knitr::plot_crop(filename)
