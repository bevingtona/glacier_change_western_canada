library(broom)
library(patchwork)

#### TOBIAS PER TYPE / YEAR ####

ref <-
  bind_rows(
    new_all_join_id_filter %>% st_drop_geometry() %>% filter(year %in% 1985) %>% 
      group_by(year, type_long) %>% 
      summarize(sum = sum(area_1985_km2, na.rm=T),
                mean = mean(area_gl_diff_1985_perc, na.rm=T)),
    new_all_join_id_filter %>% st_drop_geometry() %>% filter(year %in% 2005) %>% 
      group_by(year, type_long) %>% 
      summarize(sum = sum(area_2005_km2, na.rm=T),
                mean = mean(area_gl_diff_2005_perc, na.rm=T))) %>% 
  mutate(min = sum*0.9, 
         max = sum*1.1)

#### TOBIAS PER YEAR ####

refall <-
  bind_rows(
    new_all_join_id_filter %>% st_drop_geometry() %>% filter(year %in% 1985) %>% 
      group_by(year) %>% 
      summarize(sum = sum(area_1985_km2, na.rm=T),
                mean = mean(area_gl_diff_1985_perc, na.rm=T)),
    new_all_join_id_filter %>% st_drop_geometry() %>% filter(year %in% 2005) %>% 
      group_by(year) %>% 
      summarize(sum = sum(area_2005_km2, na.rm=T),
                mean = mean(area_gl_diff_2005_perc, na.rm=T))) %>% 
  mutate(min = sum*0.9, 
         max = sum*1.1)

#### ALL #### 

new_all_join_id_filter %>% 
  mutate(name = "All Types") %>% 
  st_drop_geometry() %>% 
  group_by(year, method, name) %>% 
  summarize(sum = sum(area_glacier_km2, na.rm = T),
            min = sum(area_glacier_km2_1sigL, na.rm = T),
            max = sum(area_glacier_km2_1sigH, na.rm = T)) %>% 
  filter(!is.na(year)) %>% 
  mutate(epoch = case_when(year <= 2010 ~ "2010", 
                           year > 2010 ~ "2020")) %>% 
  group_by(epoch) %>% 
  do(tidy = tidy(lm(sum ~ year, data = .))) %>% 
  unnest(tidy) %>% 
  filter(term == "year")

#### LM < 2010 ####

arealm2010 <-
  new_all_join_id_filter %>% 
  st_drop_geometry() %>% 
  filter(year <= 2010) %>% 
  group_by(year, type_long) %>% 
  summarize(sum = sum(area_glacier_km2, na.rm=T)) %>% 
  group_by(type_long) %>% 
  do(tidy = tidy(lm(sum ~ year, data = .))) %>% 
  unnest(tidy) %>% 
  filter(term == "year")

#### LM > 2010 ####

arealm2020 <- new_all_join_id_filter %>% 
  st_drop_geometry() %>% 
  filter(year > 2010) %>% 
  group_by(year, type_long) %>% 
  summarize(sum = sum(area_glacier_km2, na.rm=T)) %>% 
  group_by(type_long) %>% 
  do(tidy = tidy(lm(sum ~ year, data = .))) %>% 
  unnest(tidy) %>% 
  filter(term == "year")

#### ANNUAL DATA #### 

new_all_join_id_filter_year <- new_all_join_id_filter %>% 
  st_drop_geometry() %>% 
  group_by(year, type_long) %>% 
  summarize(sum = sum(area_glacier_km2, na.rm = T),
            min = sum(area_glacier_km2_1sigL, na.rm = T),
            max = sum(area_glacier_km2_1sigH, na.rm = T)) %>% 
  filter(!is.na(year))

#### PLOT ####

(ggplot(new_all_join_id_filter_year) + 
  geom_ribbon(aes(year, ymin = min, ymax = max), fill = "grey80") +
  geom_line(aes(year, sum)) + 
  geom_vline(xintercept = 2010.5, linetype = 2, color = "grey40") +
  geom_point(aes(year, sum)) +
  geom_errorbar(data = ref, aes(year, ymin = min, ymax = max), width = 1) + 
  geom_point(data = ref, aes(year, sum), shape = 21, fill = "yellow", size = 3) + 
  geom_smooth(data = . %>% filter(year <= 2010), aes(year, sum), method = "lm", se = F) +
  geom_smooth(data = . %>% filter(year > 2010), aes(year, sum), method = "lm", se = F, color = "red") +
  geom_text(data = arealm2010, aes(1995, c(17000, 600, 10000), label = paste("Slope =",signif(estimate, 3),"\n",
                                                                           "p =",signif(p.value, 2),"\n",
                                                                           "Std. error =",signif(std.error, 2))), color = "blue", size = 4) +
  geom_text(data = arealm2020, aes(2012, c(17000, 1100, 14000), label = paste("Slope =",signif(estimate, 3),"\n",
                                                                           "p =",signif(p.value, 2),"\n",
                                                                           "Std. error =",signif(std.error, 2))), color = "red", size = 4) +
   
  scale_x_continuous(expand = c(0,0)) +
  facet_wrap(.~type_long, scales = "free", ncol = 3) +
  egg::theme_article(base_size = 16) + 
  theme(aspect.ratio = 1) + 
  labs(x = "Year", y = expression(Glacier~area~(km^2)))) +
(new_all_join_id_filter %>% 
  mutate(name = "All Glaciers") %>% 
  st_drop_geometry() %>% 
  group_by(year, method, name) %>% 
  summarize(sum = sum(area_glacier_km2, na.rm = T),
            min = sum(area_glacier_km2_1sigL, na.rm = T),
            max = sum(area_glacier_km2_1sigH, na.rm = T)) %>% 
  filter(!is.na(year)) %>% 
  ggplot() + 
  geom_ribbon(aes(year, ymin = min, ymax = max), fill = "grey80") + 
  geom_line(aes(year, sum)) + 
  geom_vline(xintercept = 2010.5, linetype = 2, color = "grey40") +
  geom_point(aes(year, sum)) +
  geom_errorbar(data = refall, aes(year, ymin = min, ymax = max), width = 1) + 
  geom_point(data = refall, aes(year, sum), shape = 21, size = 3, fill = "yellow") + 
  geom_smooth(data = . %>% filter(year <= 2010), aes(year, sum), method = "lm", se = F) +
  geom_smooth(data = . %>% filter(year > 2010), aes(year, sum), method = "lm", se = F, color = "red") +
  geom_text(data = new_all_join_id_filter %>% 
              st_drop_geometry() %>% 
              filter(year <= 2010) %>% 
              group_by(year) %>% 
              summarize(sum = sum(area_glacier_km2, na.rm=T)) %>% 
              do(tidy = tidy(lm(sum ~ year, data = .))) %>% 
              unnest(tidy) %>% 
              filter(term == "year"), aes(1995, c(31000), label = paste("Slope =",signif(estimate, 3),"\n",
                                                                              "p =",signif(p.value, 2),"\n",
                                                                              "Std. error =",signif(std.error, 2))), color = "blue", size = 4) +
  geom_text(data = new_all_join_id_filter %>% 
              st_drop_geometry() %>% 
              filter(year > 2010) %>% 
              group_by(year) %>% 
              summarize(sum = sum(area_glacier_km2, na.rm=T)) %>% 
              do(tidy = tidy(lm(sum ~ year, data = .))) %>% 
              unnest(tidy) %>% 
              filter(term == "year"), aes(2015, c(31000), label = paste("Slope =",signif(estimate, 3),"\n",
                                                                               "p =",signif(p.value, 2),"\n",
                                                                               "Std. error =",signif(std.error, 2))), color = "red", size = 4) +
  scale_x_continuous(expand = c(0,0)) +
  facet_wrap(name~., scales = "free", ncol = 1) +
  egg::theme_article(base_size = 16) + 
  theme(aspect.ratio = 0.6) + 
  labs(y = expression(Glacier~area~(km^2)), x = "Year")) + plot_layout(ncol = 1, widths = c(3,1))

 ggsave("Code/3_analysis/!Figures_Final/Figure_6.tif", width = 12, height = 10, device = "tiff")
 knitr::plot_crop("Code/3_analysis/!Figures_Final/Figure_6.tif")

 
 
#### BY REGION LM ####

# Count  
mycount <- 
   bind_rows(new_all_join_id_filter %>%
  st_drop_geometry() %>%
  filter(year %in% c(1984,2010,2020)) %>%
  group_by(region, 
           type_short, 
           area_1985_km2_group, 
           year) %>%
  summarize(n = n ()) %>%
  pivot_wider(names_from = c(year),
              values_from = n) %>% 
  mutate(#dif = (`2020`-`2010`) - (`2010`-`1984`), 
         lm2010 = signif((`2010`-`1984`)/(2010-1984),2), 
         lm2020 = signif((`2020`-`2010`)/(2020-2010),2), 
         lmdif = signif(lm2020-lm2010,3)),
 new_all_join_id_filter %>%
   st_drop_geometry() %>%
   filter(year %in% c(1984,2010,2020)) %>%
   group_by(region, 
            type_short, 
            year) %>%
   summarize(n = n ()) %>%
   pivot_wider(names_from = c(year),
               values_from = n) %>% 
   mutate(#dif = (`2020`-`2010`) - (`2010`-`1984`), 
     lm2010 = signif((`2010`-`1984`)/(2010-1984),2), 
     lm2020 = signif((`2020`-`2010`)/(2020-2010),2), 
     lmdif = signif(lm2020-lm2010,3),
     area_1985_km2_group = "All sizes"))
   
# AREA TRENDS ALL 
mykmall <- new_all_join_id_filter %>% 
  st_drop_geometry() %>%
  mutate(year_group = case_when(year <= 2010 ~ "1984-2010", 
                                year >2010 ~ "2011-2020")) %>% 
  group_by(year_group, region, type_short, year) %>% 
  summarise(sum = sum(area_glacier_km2, na.rm = T)) %>% 
  do(tidy = tidy(lm(sum ~ year, data = .))) %>% 
  unnest(tidy) %>% 
  filter(term == "year") %>% 
  pivot_wider(names_from = year_group, values_from = c(estimate,std.error,p.value)) %>% 
  dplyr::select(-term, -statistic) %>%
  group_by(region, type_short) %>% 
  summarize_all(mean, na.rm = T) %>% 
  rename(LM_1984_2010 = `estimate_1984-2010`,
         LM_2011_2020 = `estimate_2011-2020`,
         SE_1984_2010 = `std.error_1984-2010`,
         SE_2011_2020 = `std.error_2011-2020`,
         P_1984_2010 = `p.value_1984-2010`,
         P_2011_2020 = `p.value_2011-2020`) %>% 
  mutate(LM_1984_2010 = signif(LM_1984_2010,2),
         SE_1984_2010 = signif(SE_1984_2010,2),
         P_1984_2010 = case_when(P_1984_2010<0.01~"**",
                                 P_1984_2010<0.05~"*", 
                                 TRUE ~ ""),
         LM_2011_2020 = signif(LM_2011_2020,2),
         SE_2011_2020 = signif(SE_2011_2020,2),
         P_2011_2020 = case_when(P_2011_2020<0.01~"**",
                                 P_2011_2020<0.05~"*", 
                                 TRUE ~ ""),
         Abs.Chg = signif(LM_2011_2020-LM_1984_2010,3),
         Perc.Chg = signif(100*(LM_2011_2020/LM_1984_2010),4)) %>% 
  mutate(LM_1984_2010 = paste(LM_1984_2010,"±", SE_1984_2010, P_1984_2010),
         LM_2011_2020 = paste(LM_2011_2020,"±", SE_2011_2020, P_2011_2020),
         Abs.Chg = paste(Abs.Chg,"±", SE_1984_2010+SE_2011_2020)) %>% 
  dplyr::select(-starts_with("SE_"), -starts_with("P_")) %>% 
  arrange(region) %>% 
  mutate(area_1985_km2_group = "All sizes")

# AREA TRENDS ALL BY SIZE
mykmsize <- new_all_join_id_filter %>% 
  st_drop_geometry() %>%
  mutate(year_group = case_when(year <= 2010 ~ "1984-2010", 
                                year >2010 ~ "2011-2020")) %>% 
  group_by(year_group, region, type_short, year, area_1985_km2_group) %>% 
  summarise(sum = sum(area_glacier_km2, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(year_group, region, type_short, area_1985_km2_group) %>% 
  do(tidy = tidy(lm(sum ~ year, data = .))) %>% 
  unnest(tidy) %>% 
  filter(term == "year") %>% 
  pivot_wider(names_from = year_group, values_from = c(estimate,std.error,p.value)) %>% 
  dplyr::select(-term, -statistic) %>%
  group_by(region, type_short, area_1985_km2_group) %>% 
  summarize_all(mean, na.rm = T) %>% 
  rename(LM_1984_2010 = `estimate_1984-2010`,
         LM_2011_2020 = `estimate_2011-2020`,
         SE_1984_2010 = `std.error_1984-2010`,
         SE_2011_2020 = `std.error_2011-2020`,
         P_1984_2010 = `p.value_1984-2010`,
         P_2011_2020 = `p.value_2011-2020`) %>% 
  mutate(LM_1984_2010 = signif(LM_1984_2010,2),
         SE_1984_2010 = signif(SE_1984_2010,2),
         P_1984_2010 = case_when(P_1984_2010<0.01~"**",
                                 P_1984_2010<0.05~"*", 
                                 TRUE ~ ""),
         LM_2011_2020 = signif(LM_2011_2020,2),
         SE_2011_2020 = signif(SE_2011_2020,2),
         P_2011_2020 = case_when(P_2011_2020<0.01~"**",
                                 P_2011_2020<0.05~"*", 
                                 TRUE ~ ""),
         Abs.Chg = signif(LM_2011_2020-LM_1984_2010,3),
         Perc.Chg = signif(100*(LM_2011_2020/LM_1984_2010),4)) %>% 
  mutate(LM_1984_2010 = paste(LM_1984_2010,"±", SE_1984_2010, P_1984_2010),
         LM_2011_2020 = paste(LM_2011_2020,"±", SE_2011_2020, P_2011_2020),
         Abs.Chg = paste(Abs.Chg,"±", SE_1984_2010+SE_2011_2020)) %>% 
  dplyr::select(-starts_with("SE_"), -starts_with("P_")) %>% 
  arrange(region)

# Area Percent Per Year (by size)
mypercall <- full_join(
  new_all_join_id_filter %>% 
    st_drop_geometry() %>%
    group_by(region, type_short, year) %>% 
    summarise(sum = sum(area_glacier_km2, na.rm = T)) %>% 
    ungroup(),
  new_all_join_id_filter %>% 
    st_drop_geometry() %>%
    group_by(region, type_short, year) %>% 
    summarise(sum = sum(area_glacier_km2, na.rm = T)) %>% 
    filter(year == 1984) %>% 
    group_by(region, type_short) %>% 
    summarize(area_1984 = max(sum)) %>%
    ungroup()) %>% 
  mutate(year_group = case_when(year <= 2010 ~ "1984-2010", 
                                year >2010 ~ "2011-2020")) %>% 
  mutate(percent = (sum/area_1984)*100) %>% 
  ungroup() %>% 
  dplyr::select(region, type_short, year_group, year, percent) %>% 
  group_by(region, type_short, year_group) %>% 
  filter(!is.na(percent)) %>% 
  do(tidy = tidy(lm(percent ~ year, data = .)))%>%
  unnest(tidy) %>% 
  filter(term == "year") %>% 
  pivot_wider(names_from = year_group, values_from = c(estimate,std.error,p.value)) %>% 
  dplyr::select(-term, -statistic) %>%
  group_by(region, type_short) %>% 
  summarize_all(mean, na.rm = T) %>% 
  rename(LMP_1984_2010 = `estimate_1984-2010`,
         LMP_2011_2020 = `estimate_2011-2020`,
         SE_1984_2010 = `std.error_1984-2010`,
         SE_2011_2020 = `std.error_2011-2020`,
         P_1984_2010 = `p.value_1984-2010`,
         P_2011_2020 = `p.value_2011-2020`) %>% 
  mutate(LMP_1984_2010 = signif(LMP_1984_2010,2),
         SE_1984_2010 = signif(SE_1984_2010,2),
         P_1984_2010 = case_when(P_1984_2010<0.01~"**",
                                 P_1984_2010<0.05~"*", 
                                 TRUE ~ ""),
         LMP_2011_2020 = signif(LMP_2011_2020,2),
         SE_2011_2020 = signif(SE_2011_2020,2),
         P_2011_2020 = case_when(P_2011_2020<0.01~"**",
                                 P_2011_2020<0.05~"*", 
                                 TRUE ~ ""),
         Abs.ChgP = signif(LMP_2011_2020-LMP_1984_2010,3),
         Perc.ChgP = signif(100*(LMP_2011_2020/LMP_1984_2010),4)) %>% 
  mutate(LMP_1984_2010 = paste(LMP_1984_2010,"±", SE_1984_2010, P_1984_2010),
         LMP_2011_2020 = paste(LMP_2011_2020,"±", SE_2011_2020, P_2011_2020),
         Abs.ChgP = paste(Abs.ChgP,"±", SE_1984_2010+SE_2011_2020)) %>% 
  dplyr::select(-starts_with("SE_"), -starts_with("P_")) %>% 
  arrange(region) %>% 
  mutate(area_1985_km2_group = "All sizes") 

# Area Percent Per Year (by size)
mypercsize <- full_join(
  new_all_join_id_filter %>% 
    st_drop_geometry() %>%
    group_by(region, type_short, year, area_1985_km2_group) %>% 
    summarise(sum = sum(area_glacier_km2, na.rm = T)) %>% 
    ungroup(),
  new_all_join_id_filter %>% 
    st_drop_geometry() %>%
    group_by(region, type_short, year, area_1985_km2_group) %>% 
    summarise(sum = sum(area_glacier_km2, na.rm = T)) %>% 
    filter(year == 1984) %>% 
    group_by(region, type_short, area_1985_km2_group) %>% 
    summarize(area_1984 = max(sum)) %>%
    ungroup()) %>% 
    mutate(year_group = case_when(year <= 2010 ~ "1984-2010", 
                                  year >2010 ~ "2011-2020")) %>% 
  mutate(percent = (sum/area_1984)*100) %>% 
  ungroup() %>% 
  dplyr::select(region, type_short, area_1985_km2_group, year_group, year, percent) %>% 
  group_by(region, type_short, year_group, area_1985_km2_group) %>% 
  filter(!is.na(percent)) %>% 
  do(tidy = tidy(lm(percent ~ year, data = .))) %>%
  unnest(tidy) %>% 
  filter(term == "year") %>% 
  pivot_wider(names_from = year_group, values_from = c(estimate,std.error,p.value)) %>% 
  dplyr::select(-term, -statistic) %>%
  group_by(region, type_short, area_1985_km2_group) %>% 
  summarize_all(mean, na.rm = T) %>% 
  rename(LMP_1984_2010 = `estimate_1984-2010`,
         LMP_2011_2020 = `estimate_2011-2020`,
         SE_1984_2010 = `std.error_1984-2010`,
         SE_2011_2020 = `std.error_2011-2020`,
         P_1984_2010 = `p.value_1984-2010`,
         P_2011_2020 = `p.value_2011-2020`) %>% 
  mutate(LMP_1984_2010 = signif(LMP_1984_2010,2),
         SE_1984_2010 = signif(SE_1984_2010,2),
         P_1984_2010 = case_when(P_1984_2010<0.01~"**",
                                 P_1984_2010<0.05~"*", 
                                 TRUE ~ ""),
         LMP_2011_2020 = signif(LMP_2011_2020,2),
         SE_2011_2020 = signif(SE_2011_2020,2),
         P_2011_2020 = case_when(P_2011_2020<0.01~"**",
                                 P_2011_2020<0.05~"*", 
                                 TRUE ~ ""),
         Abs.ChgP = signif(LMP_2011_2020-LMP_1984_2010,3),
         Perc.ChgP = signif(100*(LMP_2011_2020/LMP_1984_2010),4)) %>% 
  mutate(LMP_1984_2010 = paste(LMP_1984_2010,"±", SE_1984_2010, P_1984_2010),
         LMP_2011_2020 = paste(LMP_2011_2020,"±", SE_2011_2020, P_2011_2020),
         Abs.ChgP = paste(Abs.ChgP,"±", SE_1984_2010+SE_2011_2020)) %>% 
  dplyr::select(-starts_with("SE_"), -starts_with("P_")) %>% 
  arrange(region)

my <- full_join(
  # bind_rows(
    mycount,
    # mycount %>% 
    #   pivot_longer(c(`1984`,`2010`,`2020`)) %>% 
    #   group_by(region, type_short, name) %>% 
    #   summarize(sum = sum(value)) %>% 
    #   mutate(area_1985_km2_group = "All sizes") %>% 
    #   pivot_wider(names_from = name, values_from = sum)),
  full_join(
    bind_rows(mykmall,mykmsize),
    bind_rows(mypercall,mypercsize))
  )

my %>% write.csv("Code/3_analysis/Fig 6 Area Timeseries/Table_1.csv")

#### BY ALL LM ####

# Count  
mycount_all <- 
  bind_rows(new_all_join_id_filter %>%
              st_drop_geometry() %>%
              filter(year %in% c(1984,2010,2020)) %>%
              group_by(type_short, 
                       area_1985_km2_group, 
                       year) %>%
              summarize(n = n ()) %>%
              pivot_wider(names_from = c(year),
                          values_from = n) %>% 
              mutate(#dif = (`2020`-`2010`) - (`2010`-`1984`), 
                lm2010 = signif((`2010`-`1984`)/(2010-1984),2), 
                lm2020 = signif((`2020`-`2010`)/(2020-2010),2), 
                lmdif = signif(lm2020-lm2010,3)),
            new_all_join_id_filter %>%
              st_drop_geometry() %>%
              filter(year %in% c(1984,2010,2020)) %>%
              group_by(type_short, 
                       year) %>%
              summarize(n = n ()) %>%
              pivot_wider(names_from = c(year),
                          values_from = n) %>% 
              mutate(#dif = (`2020`-`2010`) - (`2010`-`1984`), 
                lm2010 = signif((`2010`-`1984`)/(2010-1984),2), 
                lm2020 = signif((`2020`-`2010`)/(2020-2010),2), 
                lmdif = signif(lm2020-lm2010,3),
                area_1985_km2_group = "All sizes"))

# AREA TRENDS ALL 
mykmall_all <- new_all_join_id_filter %>% 
  st_drop_geometry() %>%
  mutate(year_group = case_when(year <= 2010 ~ "1984-2010", 
                                year >2010 ~ "2011-2020")) %>% 
  group_by(year_group, type_short, year) %>% 
  summarise(sum = sum(area_glacier_km2, na.rm = T)) %>% 
  do(tidy = tidy(lm(sum ~ year, data = .))) %>% 
  unnest(tidy) %>% 
  filter(term == "year") %>% 
  pivot_wider(names_from = year_group, values_from = c(estimate,std.error,p.value)) %>% 
  dplyr::select(-term, -statistic) %>%
  group_by(type_short) %>% 
  summarize_all(mean, na.rm = T) %>% 
  rename(LM_1984_2010 = `estimate_1984-2010`,
         LM_2011_2020 = `estimate_2011-2020`,
         SE_1984_2010 = `std.error_1984-2010`,
         SE_2011_2020 = `std.error_2011-2020`,
         P_1984_2010 = `p.value_1984-2010`,
         P_2011_2020 = `p.value_2011-2020`) %>% 
  mutate(LM_1984_2010 = signif(LM_1984_2010,2),
         SE_1984_2010 = signif(SE_1984_2010,2),
         P_1984_2010 = case_when(P_1984_2010<0.01~"**",
                                 P_1984_2010<0.05~"*", 
                                 TRUE ~ ""),
         LM_2011_2020 = signif(LM_2011_2020,2),
         SE_2011_2020 = signif(SE_2011_2020,2),
         P_2011_2020 = case_when(P_2011_2020<0.01~"**",
                                 P_2011_2020<0.05~"*", 
                                 TRUE ~ ""),
         Abs.Chg = signif(LM_2011_2020-LM_1984_2010,3),
         Perc.Chg = signif(100*(LM_2011_2020/LM_1984_2010),4)) %>% 
  mutate(LM_1984_2010 = paste(LM_1984_2010,"±", SE_1984_2010, P_1984_2010),
         LM_2011_2020 = paste(LM_2011_2020,"±", SE_2011_2020, P_2011_2020),
         Abs.Chg = paste(Abs.Chg,"±", SE_1984_2010+SE_2011_2020)) %>% 
  dplyr::select(-starts_with("SE_"), -starts_with("P_")) %>% 
  mutate(area_1985_km2_group = "All sizes")

# AREA TRENDS ALL BY SIZE
mykmsize_all <- new_all_join_id_filter %>% 
  st_drop_geometry() %>%
  mutate(year_group = case_when(year <= 2010 ~ "1984-2010", 
                                year >2010 ~ "2011-2020")) %>% 
  group_by(year_group, type_short, year, area_1985_km2_group) %>% 
  summarise(sum = sum(area_glacier_km2, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(year_group, type_short, area_1985_km2_group) %>% 
  do(tidy = tidy(lm(sum ~ year, data = .))) %>% 
  unnest(tidy) %>% 
  filter(term == "year") %>% 
  pivot_wider(names_from = year_group, values_from = c(estimate,std.error,p.value)) %>% 
  dplyr::select(-term, -statistic) %>%
  group_by(type_short, area_1985_km2_group) %>% 
  summarize_all(mean, na.rm = T) %>% 
  rename(LM_1984_2010 = `estimate_1984-2010`,
         LM_2011_2020 = `estimate_2011-2020`,
         SE_1984_2010 = `std.error_1984-2010`,
         SE_2011_2020 = `std.error_2011-2020`,
         P_1984_2010 = `p.value_1984-2010`,
         P_2011_2020 = `p.value_2011-2020`) %>% 
  mutate(LM_1984_2010 = signif(LM_1984_2010,2),
         SE_1984_2010 = signif(SE_1984_2010,2),
         P_1984_2010 = case_when(P_1984_2010<0.01~"**",
                                 P_1984_2010<0.05~"*", 
                                 TRUE ~ ""),
         LM_2011_2020 = signif(LM_2011_2020,2),
         SE_2011_2020 = signif(SE_2011_2020,2),
         P_2011_2020 = case_when(P_2011_2020<0.01~"**",
                                 P_2011_2020<0.05~"*", 
                                 TRUE ~ ""),
         Abs.Chg = signif(LM_2011_2020-LM_1984_2010,3),
         Perc.Chg = signif(100*(LM_2011_2020/LM_1984_2010),4)) %>% 
  mutate(LM_1984_2010 = paste(LM_1984_2010,"±", SE_1984_2010, P_1984_2010),
         LM_2011_2020 = paste(LM_2011_2020,"±", SE_2011_2020, P_2011_2020),
         Abs.Chg = paste(Abs.Chg,"±", SE_1984_2010+SE_2011_2020)) %>% 
  dplyr::select(-starts_with("SE_"), -starts_with("P_"))

# Area Percent Per Year (by size)
mypercall_all <- full_join(
  new_all_join_id_filter %>% 
    st_drop_geometry() %>%
    group_by(type_short, year) %>% 
    summarise(sum = sum(area_glacier_km2, na.rm = T)) %>% 
    ungroup(),
  new_all_join_id_filter %>% 
    st_drop_geometry() %>%
    group_by(type_short, year) %>% 
    summarise(sum = sum(area_glacier_km2, na.rm = T)) %>% 
    filter(year == 1984) %>% 
    group_by(type_short) %>% 
    summarize(area_1984 = max(sum)) %>%
    ungroup()) %>% 
  mutate(year_group = case_when(year <= 2010 ~ "1984-2010", 
                                year >2010 ~ "2011-2020")) %>% 
  mutate(percent = (sum/area_1984)*100) %>% 
  ungroup() %>% 
  dplyr::select(type_short, year_group, year, percent) %>% 
  group_by(type_short, year_group) %>% 
  filter(!is.na(percent)) %>% 
  do(tidy = tidy(lm(percent ~ year, data = .)))%>%
  unnest(tidy) %>% 
  filter(term == "year") %>% 
  pivot_wider(names_from = year_group, values_from = c(estimate,std.error,p.value)) %>% 
  dplyr::select(-term, -statistic) %>%
  group_by(type_short) %>% 
  summarize_all(mean, na.rm = T) %>% 
  rename(LMP_1984_2010 = `estimate_1984-2010`,
         LMP_2011_2020 = `estimate_2011-2020`,
         SE_1984_2010 = `std.error_1984-2010`,
         SE_2011_2020 = `std.error_2011-2020`,
         P_1984_2010 = `p.value_1984-2010`,
         P_2011_2020 = `p.value_2011-2020`) %>% 
  mutate(LMP_1984_2010 = signif(LMP_1984_2010,2),
         SE_1984_2010 = signif(SE_1984_2010,2),
         P_1984_2010 = case_when(P_1984_2010<0.01~"**",
                                 P_1984_2010<0.05~"*", 
                                 TRUE ~ ""),
         LMP_2011_2020 = signif(LMP_2011_2020,2),
         SE_2011_2020 = signif(SE_2011_2020,2),
         P_2011_2020 = case_when(P_2011_2020<0.01~"**",
                                 P_2011_2020<0.05~"*", 
                                 TRUE ~ ""),
         Abs.ChgP = signif(LMP_2011_2020-LMP_1984_2010,3),
         Perc.ChgP = signif(100*(LMP_2011_2020/LMP_1984_2010),4)) %>% 
  mutate(LMP_1984_2010 = paste(LMP_1984_2010,"±", SE_1984_2010, P_1984_2010),
         LMP_2011_2020 = paste(LMP_2011_2020,"±", SE_2011_2020, P_2011_2020),
         Abs.ChgP = paste(Abs.ChgP,"±", SE_1984_2010+SE_2011_2020)) %>% 
  dplyr::select(-starts_with("SE_"), -starts_with("P_")) %>% 
  mutate(area_1985_km2_group = "All sizes") 

# Area Percent Per Year (by size)
mypercsize_all <- full_join(
  new_all_join_id_filter %>% 
    st_drop_geometry() %>%
    group_by(type_short, year, area_1985_km2_group) %>% 
    summarise(sum = sum(area_glacier_km2, na.rm = T)) %>% 
    ungroup(),
  new_all_join_id_filter %>% 
    st_drop_geometry() %>%
    group_by(type_short, year, area_1985_km2_group) %>% 
    summarise(sum = sum(area_glacier_km2, na.rm = T)) %>% 
    filter(year == 1984) %>% 
    group_by(type_short, area_1985_km2_group) %>% 
    summarize(area_1984 = max(sum)) %>%
    ungroup()) %>% 
  mutate(year_group = case_when(year <= 2010 ~ "1984-2010", 
                                year >2010 ~ "2011-2020")) %>% 
  mutate(percent = (sum/area_1984)*100) %>% 
  ungroup() %>% 
  dplyr::select(type_short, area_1985_km2_group, year_group, year, percent) %>% 
  group_by(type_short, year_group, area_1985_km2_group) %>% 
  filter(!is.na(percent)) %>% 
  do(tidy = tidy(lm(percent ~ year, data = .))) %>%
  unnest(tidy) %>% 
  filter(term == "year") %>% 
  pivot_wider(names_from = year_group, values_from = c(estimate,std.error,p.value)) %>% 
  dplyr::select(-term, -statistic) %>%
  group_by(type_short, area_1985_km2_group) %>% 
  summarize_all(mean, na.rm = T) %>% 
  rename(LMP_1984_2010 = `estimate_1984-2010`,
         LMP_2011_2020 = `estimate_2011-2020`,
         SE_1984_2010 = `std.error_1984-2010`,
         SE_2011_2020 = `std.error_2011-2020`,
         P_1984_2010 = `p.value_1984-2010`,
         P_2011_2020 = `p.value_2011-2020`) %>% 
  mutate(LMP_1984_2010 = signif(LMP_1984_2010,2),
         SE_1984_2010 = signif(SE_1984_2010,2),
         P_1984_2010 = case_when(P_1984_2010<0.01~"**",
                                 P_1984_2010<0.05~"*", 
                                 TRUE ~ ""),
         LMP_2011_2020 = signif(LMP_2011_2020,2),
         SE_2011_2020 = signif(SE_2011_2020,2),
         P_2011_2020 = case_when(P_2011_2020<0.01~"**",
                                 P_2011_2020<0.05~"*", 
                                 TRUE ~ ""),
         Abs.ChgP = signif(LMP_2011_2020-LMP_1984_2010,3),
         Perc.ChgP = signif(100*(LMP_2011_2020/LMP_1984_2010),4)) %>% 
  mutate(LMP_1984_2010 = paste(LMP_1984_2010,"±", SE_1984_2010, P_1984_2010),
         LMP_2011_2020 = paste(LMP_2011_2020,"±", SE_2011_2020, P_2011_2020),
         Abs.ChgP = paste(Abs.ChgP,"±", SE_1984_2010+SE_2011_2020)) %>% 
  dplyr::select(-starts_with("SE_"), -starts_with("P_"))

my_all <- full_join(
  # bind_rows(
  mycount_all,
  # mycount %>% 
  #   pivot_longer(c(`1984`,`2010`,`2020`)) %>% 
  #   group_by(region, type_short, name) %>% 
  #   summarize(sum = sum(value)) %>% 
  #   mutate(area_1985_km2_group = "All sizes") %>% 
  #   pivot_wider(names_from = name, values_from = sum)),
  full_join(
    bind_rows(mykmall_all,mykmsize_all),
    bind_rows(mypercall_all,mypercsize_all))
)

my_all %>% write.csv("Code/3_analysis/Table/Table_1_all.csv")


#### ALL LM ####

# new_all_join_id_filter %>% 
#   st_drop_geometry() %>% 
#   mutate(year_group = case_when(year <= 2010 ~ "1984-2010", 
#                                 year >2010 ~ "2011-2020")) %>% 
#   group_by(year, type_short, year_group) %>% 
#   summarize(sum = sum(area_glacier_km2, na.rm = T)) %>% 
#   group_by(type_short, year_group) %>% 
#   do(tidy = tidy(lm(sum ~ year, data = .))) %>% 
#   unnest(tidy) %>% 
#   filter(term == "year") %>% 
#   pivot_wider(names_from = year_group, values_from = c(estimate,std.error,p.value)) %>% 
#   dplyr::select(-term, -statistic) %>%
#   group_by(type_short) %>% 
#   summarize_all(mean, na.rm = T) %>% 
#   rename(LM_1984_2010 = `estimate_1984-2010`,
#          LM_2011_2020 = `estimate_2011-2020`,
#          SE_1984_2010 = `std.error_1984-2010`,
#          SE_2011_2020 = `std.error_2011-2020`,
#          P_1984_2010 = `p.value_1984-2010`,
#          P_2011_2020 = `p.value_2011-2020`) %>% 
#   mutate(LM_1984_2010 = signif(LM_1984_2010,3),
#          SE_1984_2010 = signif(SE_1984_2010,3),
#          P_1984_2010 = case_when(P_1984_2010<0.01~"**",
#                                  P_1984_2010<0.05~"*", 
#                                  TRUE ~ ""),
#          LM_2011_2020 = signif(LM_2011_2020,3),
#          SE_2011_2020 = signif(SE_2011_2020,3),
#          P_2011_2020 = case_when(P_2011_2020<0.01~"**",
#                                  P_2011_2020<0.05~"*", 
#                                  TRUE ~ ""),
#          Abs.Chg = signif(LM_2011_2020-LM_1984_2010,3),
#          Perc.Chg = signif(100*(LM_2011_2020/LM_1984_2010),4)) %>% 
#   mutate(LM_1984_2010 = paste(LM_1984_2010,"±", SE_1984_2010, P_1984_2010),
#          LM_2011_2020 = paste(LM_2011_2020,"±", SE_2011_2020, P_2011_2020),
#          Abs.Chg = paste(Abs.Chg,"±", SE_1984_2010+SE_2011_2020)) %>% 
#   dplyr::select(-starts_with("SE_"), -starts_with("P_")) 
#  %>% 
#   write.csv("Code/3_analysis/Fig 7 Area Timeseries/Area_ts.csv")

#### LM > 2010 ####

# arealm2020 <- new_all_join_id_filter %>% 
#   st_drop_geometry() %>% 
#   filter(year >= 2010) %>% 
#   group_by(year, type_long) %>% 
#   summarize(sum = sum(area_2005_km2, na.rm=T)) %>% 
#   group_by(type_long) %>% 
#   do(tidy = tidy(lm(sum ~ year, data = .))) %>% 
#   unnest(tidy) %>% 
#   filter(term == "year")


# # 1985 manual vs auto point
# # area1985 <- 
#   new_all_join_id_filter %>% 
#   st_drop_geometry() %>% 
#   filter(method == "moving") %>% 
#   filter(year == 1985) %>% 
#   group_by(year, type_long) %>% 
#   summarise(manual = sum(area_1985_km2, na.rm = T),
#             manualmin = manual*0.85,
#             manualmax = manual*1.15,
#             auto = sum(area_glacier_km2, na.rm = T)#,
#             # automin = sum(min_area, na.rm = T),
#             # automax = sum(max_area, na.rm = T)
#             ) 
#   %>% 
#   mutate(perc = round(-1*100*(-1+auto / manual),1))
# 
# # 2005 manual vs auto point
# area2005 <- new_all_drop_moving %>% 
#   filter(year == 2005) %>% 
#   group_by(year, type_long) %>% 
#   summarise(manual = sum(area_2005_km2, na.rm = T),
#             manualmin = manual*0.85,
#             manualmax = manual*1.15,
#             auto = sum(area_km2, na.rm = T),
#             automin = sum(min_area, na.rm = T),
#             automax = sum(max_area, na.rm = T)) %>% 
#   mutate(perc = round(-1*100*(-1+auto / manual),1))
# 
# # Timeseries with standard error
# area_ts <- new_all_join_id_filter %>% 
#   st_drop_geometry() %>% 
#   # filter(method == "moving") %>%
#   group_by(year, type_long) %>% 
#   summarise(sum = sum(area_glacier_km2, na.rm = T), 
#             max = sum(area_glacier_km2_2sigL, na.rm = T),
#             min = sum(area_glacier_km2_2sigH, na.rm = T)) 
# 
# # LM < 2010
# arealm2010 <- area_ts %>% 
#   filter(year <= 2010) %>% 
#   group_by(type_long) %>% 
#   do(tidy = tidy(lm(sum ~ year, data = .))) %>% 
#   unnest(tidy) %>% 
#   filter(term == "year")
# 
# # LM > 2010
# arealm2019 <- area_ts %>% 
#   filter(year >= 2010) %>% 
#   group_by(type_long) %>% 
#   do(tidy = tidy(lm(sum ~ year, data = .))) %>% 
#   unnest(tidy) %>% 
#   filter(term == "year")
# 
# # PLOTSICLE
# ggplot() + 
#   
#   geom_ribbon(data = area_ts, aes(x = year, ymin = min/1000, ymax = max/1000), alpha = 0.15, colour = NA) +
#   geom_line(data = area_ts, aes(year, sum/1000), size = 2) + 
#   
#   # geom_errorbar(data = area1985, aes(year, manual/1000, ymin = manual/1000 * 0.9, ymax = manual/1000 * 1.1)) +
#   # geom_point(data = area1985, aes(year, manual/1000, fill = "Bolch et al. (2010)"), shape = 22, size = 3) +
#   # 
#   # geom_errorbar(data = area2005, aes(year, manual/1000, ymin = manual/1000 * 0.9, ymax = manual/1000 * 1.1)) +
#   # geom_point(data = area2005, aes(year, manual/1000, fill = "Bolch et al. (2010)"), shape = 22, size = 3) +
#   
#   scale_fill_manual(values = "yellow") + 
# 
#   geom_smooth(data = area_ts %>% filter(year <= 2010), aes(year, sum/1000), method = "lm", se = F, color = "blue", size = 1) +
#   geom_smooth(data = area_ts %>% filter(year >= 2010), aes(year, sum/1000), method = "lm", se = F, color = "red", size = 1) +
# 
#   # geom_text_repel(data = area1985, aes(year, manual/1000, label = paste(perc,"%")), nudge_x = 5) +
#   # geom_text_repel(data = area2005, aes(year, manual/1000, label = paste(perc,"%")), box.padding = 2) +
#   
#   # geom_text(data = arealm2010, aes(1992, c(13.5, 0.18, 13.4), label = paste("Slope =",signif(estimate, 2),"\n",
#   #                                                                           "p =",signif(p.value, 2),"\n",
#   #                                                                           "Std. error =",signif(std.error, 2))), color = "blue", size = 2) +
#   # geom_text(data = arealm2019, aes(2012, c(11.5, 0.15, 12.5), label = paste("Slope =",signif(estimate, 2),"\n",
#   #                                                                           "p =",signif(p.value, 2),"\n",
#   #                                                                           "Std. error =",signif(std.error, 2))), color = "red", size = 2) +
#   
#   facet_wrap(~type_long, scales = "free") + 
#   egg::theme_article() + 
#   
#   theme(aspect.ratio = 1, legend.position = c(0.9,0.1)) +
#   
#   labs(x = "Year", 
#        y = expression("Glacier Area "~(km^2)~"x1000"), 
#        fill = "")
# 
# filename <- paste0("2_Paper_BCA_Glacier/2_R_Code/4_thesis_figures/Fig 7 Area Timeseries/", 
#                    format(lubridate::now(), "%Y%m%d-%H%M-"),"Fig_7_Area.tiff")
# ggsave(filename = filename, device = "tiff", height = 10, width = 10)
# knitr::plot_crop(filename)
