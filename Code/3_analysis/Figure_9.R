#### LAKE STATS ####

lakes <- full_join(
  new_all_join_id_filter %>% 
    st_drop_geometry() %>% 
    group_by(year) %>% 
    summarise(sum = sum(area_water_km2, na.rm = T)),
  new_all_join_id_filter %>% 
    st_drop_geometry() %>% 
    mutate(wa = area_water_km2 > 0.01) %>% 
    filter(wa == T) %>%  
    group_by(year) %>% 
    summarise(n = n()))

mod2010 <- lakes %>% 
  filter(year <= 2010) %>% 
  do(tidy = tidy(lm(sum ~ year, data = .))) %>% 
  unnest(tidy) %>% 
  filter(term == "year")

mod2019 <- lakes %>% 
  filter(year > 2010) %>% 
  do(tidy = tidy(lm(sum ~ year, data = .))) %>% 
  unnest(tidy) %>% 
  filter(term == "year")

lakes %>% filter(year == 2020)

ggplot() + 
  # geom_ribbon(data = lakes, aes(year, ymin = sum*0.8, ymax = sum *1.2), fill = "grey80") + 
  geom_point(data = lakes, aes(year, sum)) + 
  geom_smooth(data = lakes %>% filter(year <= 2010), aes(year, sum), method = "lm", se = F, color = "blue") +
  geom_smooth(data = lakes %>% filter(year >= 2010), aes(year, sum), method = "lm", se = F, color = "red") +
  scale_size_binned(range = c(1,10), n.breaks = 10) + 
  geom_text(data = mod2010, aes(1989, c(200), label = paste("Slope =",signif(estimate, 2),"\n",
                                                                          "p =",signif(p.value, 2),"\n",
                                                                          "Std. error =",signif(std.error, 2))), color = "blue", size = 3) +
  geom_text(data = mod2019, aes(2006, c(400), label = paste("Slope =",signif(estimate, 2),"\n",
                                                                          "p =",signif(p.value, 2),"\n",
                                                                          "Std. error =",signif(std.error, 2))), color = "red", size = 3) +
  egg::theme_article() +
  theme(aspect.ratio = 1) +
  labs(x = "Year", y = expression ("Proglacial lake area"~(km^2)), 
       size = "Number of Lakes") 

filename <- paste0("Code/3_analysis/!Figures_Final/Figure_9.tiff")
ggsave(filename = filename, device = "tiff", height = 5, width = 5, dpi = 300)
knitr::plot_crop(filename)
