#### FRAGMENTATION ####

library(broom)

frag_data <-
  new_all_join_id_filter %>% 
    st_drop_geometry() %>%  
    group_by(type_long) %>% 
    mutate(sum_all = length(unique(n))) %>% 
    group_by(year, type_long, sum_all) %>% 
    summarise(sum = sum(nparts, na.rm = T),
              mean = mean(nparts, na.rm = T)) 

frag2010 <- frag_data %>% 
  filter(year <= 2010) %>% 
  group_by(type_long) %>% 
  do(tidy = tidy(lm(sum ~ year, data = .))) %>% 
  unnest(tidy) %>% 
  filter(term == "year")
frag2019 <-
  frag_data %>% 
  filter(year > 2010) %>% 
  group_by(type_long) %>% 
  do(tidy = tidy(lm(sum ~ year, data = .))) %>% 
  unnest(tidy) %>% 
  filter(term == "year")



frag_data %>% 
  group_by(type_long) %>% 
  summarise(sd = sd(sum))

frag_data %>% 
  ggplot() + 
  # geom_ribbon(aes(x = year, ymin = sum-c(496,16,128), ymax = sum+c(496,16,128), group = type_long), fill = "grey80") +
  geom_point(aes(year, sum), fill = "light blue") +
  geom_smooth(data = frag_data %>% filter(year <= 2010), aes(year, sum), method = "lm", se = F, color = "blue") +
  geom_smooth(data = frag_data %>% filter(year >= 2010), aes(year, sum), method = "lm", se = F, color = "red") +
  geom_text(data = frag2010, aes(1990, c(14900, 250, 1600), label = paste("Slope =",signif(estimate, 2),"\n",
                                                                        "p =",signif(p.value, 2),"\n",
                                                                        "Std. error =",signif(std.error, 2))), color = "blue", size = 3) +
  geom_text(data = frag2019, aes(2006, c(15500, 280, 1800), label = paste("Slope =",signif(estimate, 2),"\n",
                                                                          "p =",signif(p.value, 2),"\n",
                                                                          "Std. error =",signif(std.error, 2))), color = "red", size = 3) +
  scale_size_binned(range = c(1,10), n.breaks = 5, limits = c(1,2)) +
  facet_wrap(~type_long, scales = "free") + 
  egg::theme_article() + 
  theme(aspect.ratio = 1) + 
  labs(x = "Year", y = "Number of Glacier Fragments", size = "Average number \nof parts")

filename <- paste0("Code/3_analysis/!Figures_Final/Figure_7.tif")
ggsave(filename = filename, device = "tiff", height = 10, width = 10, dpi = 300)
knitr::plot_crop(filename)

