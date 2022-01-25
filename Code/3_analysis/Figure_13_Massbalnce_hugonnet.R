library(tidyverse)

# df <- read.csv("Data/hugonnet_et_al.csv") %>% 
#   rename(region = ï..region) %>% 
#   filter(include == "Y") %>% 
#   dplyr::select(-include,-area_km2, -starts_with("budget")) %>% 
#   pivot_longer(cols = c(-region), names_to = "var", values_to = "val") %>% 
#   mutate(era = str_split(var, "_", 2, simplify = T)[,1],
#          stat = str_split(var, "_", 2, simplify = T)[,2]) %>% 
#   dplyr::select(-var)
# 
# gl <- bind_rows(
#   new_all_join_id_filter %>% 
#     st_drop_geometry() %>% 
#     mutate(era = case_when(year %in% 2000:2010 ~ "early",
#                            year %in% 2010:2020 ~ "late", 
#                            TRUE ~ "N")) %>% 
#     filter(era != "N") %>% 
#     group_by(era, region, year) %>% 
#     summarize(sum = sum(area_glacier_km2)) %>% 
#     do(tidy = tidy(lm(sum ~ year, data = .))) %>% 
#     unnest(tidy) %>% 
#     filter(term == "year"),
#   new_all_join_id_filter %>% 
#     st_drop_geometry() %>% 
#     mutate(era = case_when(year %in% 2000:2020 ~ "full",
#                            TRUE ~ "N")) %>% 
#     filter(era != "N") %>% 
#     group_by(era, region, year) %>% 
#     summarize(sum = sum(area_glacier_km2)) %>% 
#     do(tidy = tidy(lm(sum ~ year, data = .))) %>% 
#     unnest(tidy) %>% 
#     filter(term == "year"))
# 
# 
st_crs(new_all_join_id_filter) <- 3005

# HUGONNET RESULTS 
df <-
  read.csv("Data/hugonnet_et_al.csv") %>% 
  rename(region = ï..region) %>% 
  filter(include == "Y") %>% 
  dplyr::select(region, starts_with("budget")) %>% 
  pivot_longer(cols = c(-region), names_to = "var", values_to = "val") %>% 
  mutate(era = str_split(var, "_", 2, simplify = T)[,1],
         stat = str_split(var, "_", 2, simplify = T)[,2]) %>% 
  dplyr::select(-var) %>% 
  mutate(era = sub("budget","full",era)) %>% 
  pivot_wider(id_cols = region, names_from = stat, values_from = val)

# HUGONNET AREAS 
regions_hg <- read_sf("Data/Regions_BC_YK.shp") %>% 
  mutate(id = row_number(), area = st_area(.)) %>% 
  st_transform(3005)

regions_hg <- regions_hg %>% 
  mutate(region = case_when(id == 1 ~ "SEM", 
                            id == 2 ~ "VIL", 
                            id == 3 ~ "SIR", 
                            id == 4 ~ "NRM", 
                            id == 5 ~ "CCM", 
                            id == 6 ~ "SCM", 
                            id == 7 ~ "NIR",
                            id == 8 ~ "CRM",
                            id == 9 ~ "SRM",
                            id == 10 ~ "NAE",
                            id == 11 ~ "SEY",
                            id == 12 ~ "NAW",
                            id == 13 ~ "NCM",
                            TRUE ~ NA_character_)) %>% 
  filter(region %in% df$region) %>% dplyr::select(region)

# CLIP GLACIERS TO HUGONNET AREAS 

rm(all)

plan(multisession)

all <- do.call(bind_rows, future_lapply(regions$region, function(nn){
  
  r <- regions_hg %>% filter(region == nn)
  print(nn)

  years <- do.call(bind_rows, lapply(1984:2020, function(y){
    
    print(paste(nn,y))
    
    g <-
      new_all_join_id_filter %>% 
        filter(region == nn, year == y) %>% 
      st_intersection(r) %>% 
      mutate(area_glacier_km2_hg = as.numeric(st_area(.))/(1000*1000), 
             dif = area_glacier_km2_hg-area_glacier_km2)
    
    return(g)
    
  }))
  
  return(years)
  }))




# FIGURE - BEVINGTON PAPER (BOLCH REGIONS)

# gl <- all %>% 
#   st_drop_geometry() %>% 
#   mutate(era = case_when(year %in% 2000:2020 ~ "full",
#                          TRUE ~ "N")) %>% 
#   filter(era != "N") %>% 
#   group_by(era, region, year) %>% 
#   summarize(sum = sum(area_glacier_km2)) %>% #area_glacier_km2
#   do(tidy = tidy(lm(sum ~ year, data = .))) %>% 
#   unnest(tidy) %>% 
#   filter(term == "year")
# 
# d <- full_join(df, gl) %>% 
#   mutate(era = case_when(era == "early" ~ "2000-2010",
#                          era == "late" ~ "2010-2020",
#                          era == "full" ~ "2000-2020")) %>% 
#   mutate(era = factor(era, levels=c("2000-2010","2010-2020","2000-2020"))) 
#   
# d %>% ggplot() + 
#   geom_rect(aes(ymin = lm - se, ymax = lm + se, 
#                 xmin = estimate - std.error, 
#                 xmax = estimate + std.error, fill = region), alpha = 0.5, 
#             show.legend = F) + 
#   geom_point(aes(estimate, lm, color = region), show.legend = F) + 
#   geom_smooth(aes(estimate, lm), method = "lm", se = F, color = "grey", show.legend = F) +
#   facet_wrap(~era, scales = "free") + 
#   theme(aspect.ratio = 1) +
#   scale_x_continuous(minor_breaks = seq(-100,100,10)) +
#   ggrepel::geom_text_repel(aes(estimate, lm, label = region, color = region), show.legend = F) + 
#   labs(x = expression(Glacier~area~(km^2~a^-1)), 
#        y = expression(Mass~budget~(Gt~a^-1))) +
#   egg::theme_article() + 
#   theme(aspect.ratio = 1) + 
#   scale_x_continuous(limits = c(-85,5))
# 
# ggsave("Code/3_analysis/Fig_MB/area_vs_mass_regions_bolch.jpg")
# knitr::plot_crop("Code/3_analysis/Fig_MB/area_vs_mass_regions_bolch.jpg")

# FIGURE - BEVINGTON PAPER (BOLCH REGIONS)

gl <- all %>% 
    st_drop_geometry() %>% 
    mutate(era = case_when(year %in% 2000:2020 ~ "full",
                           TRUE ~ "N")) %>% 
    filter(era != "N") %>% 
    group_by(era, region, year) %>% 
    summarize(sum = sum(area_glacier_km2_hg)) %>% #area_glacier_km2
    do(tidy = tidy(lm(sum ~ year, data = .))) %>% 
    unnest(tidy) %>% 
    filter(term == "year")
  

d <- full_join(df, gl) %>% 
  mutate(era = case_when(era == "early" ~ "2000-2010",
                         era == "late" ~ "2010-2020",
                         era == "full" ~ "2000-2020")) %>% 
  mutate(era = factor(era, levels=c("2000-2010","2010-2020","2000-2020"))) 


l <- lm(lm ~ estimate, data = d)


ti <- tidy(lm(lm ~ estimate, data = d)) %>% filter(term == "estimate")
gl <- glance(lm(lm ~ estimate, data = d))

d %>% ggplot() + 
  # geom_abline(slope = 1, intercept = 0) +
  geom_smooth(aes(-estimate, -lm), method = "lm", se = F, color = "grey", show.legend = F, 
              linetype = 2, alpha = 0.6) +
  geom_rect(aes(ymin = -lm - se, 
                ymax = -lm + se,
                xmin = -estimate - std.error,
                xmax = -estimate + std.error, color = region), alpha = 0.5, fill = NA,
            show.legend = F) +
  # annotate(geom = "text", label  = 
  #            paste0("Slope = ",signif(ti$estimate, 2), "\n",
  #                   "r2 = ", signif(gl$adj.r.squared, 2),
  #                   "\np-value = ",signif(ti$p.value, 2)),
  #          color = "red",
  #          hjust = 0,
  #          x = 0.5, y = 100) +
  
  facet_wrap(~era, scales = "free") +
  theme(aspect.ratio = 1) +
  ggrepel::geom_text_repel(aes(-estimate, -lm, label = region, color = region), 
                           show.legend = F, box.padding = 1) + 
  labs(x = expression(Glacier~area~loss~(km^2~a^-1)), 
       y = expression(Mass~budget~loss~(Gt~a^-1))) +
  egg::theme_article() +
  # theme(axis.line = element_line(color = "black")) +
  theme(aspect.ratio = 1) +
  scale_x_log10(breaks = c(0.1, 1, 10, 100), limits = c(0.3,100)) +
  scale_y_log10() + 
  annotation_logticks()



  
ggsave("Code/3_analysis/!Figures_Final/Figure_13.tif", device = "tiff", width = 8, height = 8)
knitr::plot_crop("Code/3_analysis/!Figures_Final/Figure_13.tif")


all %>% 
  st_drop_geometry() %>% 
  # filter(year %in% 2000:2020) %>% 
  dplyr::select(-region.1,-dif) %>% 
  group_by(region, year) %>% 
  summarize(area_glacier_km2 = sum(area_glacier_km2),
            area_glacier_km2_hg = sum(area_glacier_km2_hg)) %>% 
  write_csv("Code/3_analysis/Fig_MB/area_over_time_bolch_vs_hg_regions_1984_2020.csv")
  


hg_all_era <- bind_rows(
  # Area Change (decade)
  all %>% 
    st_drop_geometry() %>% 
    mutate(era = case_when(year %in% 2000:2010 ~ "early",
                           year %in% 2010:2020 ~ "late", 
                           TRUE ~ "N")) %>% 
    filter(era != "N") %>% 
    group_by(era, region, year) %>% 
    summarize(sum = sum(area_glacier_km2_hg)) %>% 
    do(tidy = tidy(lm(sum ~ year, data = .))) %>% 
    unnest(tidy) %>% 
    filter(term == "year"),
  # Area Change (full)
  all %>% 
    st_drop_geometry() %>% 
    mutate(era = case_when(year %in% 2000:2020 ~ "full",
                           TRUE ~ "N")) %>% 
    filter(era != "N") %>% 
    group_by(era, region, year) %>% 
    summarize(sum = sum(area_glacier_km2_hg)) %>% 
    do(tidy = tidy(lm(sum ~ year, data = .))) %>% 
    unnest(tidy) %>% 
    filter(term == "year"))

d <- full_join(df, hg_all_era) %>% 
  mutate(era = case_when(era == "early" ~ "2000-2010",
                         era == "late" ~ "2010-2020",
                         era == "full" ~ "2000-2020")) %>% 
  mutate(era = factor(era, levels=c("2000-2010","2010-2020","2000-2020"))) 


d %>% ggplot() + 
  geom_rect(aes(ymin = lm - se, ymax = lm + se, 
                xmin = estimate - std.error, 
                xmax = estimate + std.error, fill = region), alpha = 0.5, 
            show.legend = F) + 
  geom_point(aes(estimate, lm, color = region), show.legend = F) + 
  geom_smooth(aes(estimate, lm), method = "lm", se = F, color = "grey", show.legend = F) +
  facet_wrap(~era, scales = "free") + 
  theme(aspect.ratio = 1) +
  scale_x_continuous(minor_breaks = seq(-100,100,10)) +
  ggrepel::geom_text_repel(aes(estimate, lm, label = region, color = region), show.legend = F) + 
  labs(x = expression(Glacier~area~(km^2~a^-1)), 
       y = expression(Mass~budget~(Gt~a^-1))) +
  egg::theme_article() + 
  theme(aspect.ratio = 1) 
+ 
  scale_x_continuous(limits = c(-85,5))




alex <- read_csv("Code/3_analysis/Fig_MB/area_over_time_bolch_vs_hg_regions.csv")
mass_change <- read_csv("Code/3_analysis/Fig_MB/hugonnet_et_al.csv")

alex %>% 
  filter(year %in% 2000:2010) %>% 
  group_by(region) %>% 
  do(tidy = tidy(lm(area_glacier_km2_hg ~ year, data = .))) %>% 
  unnest(tidy) %>% 
  filter(term == "year") %>% 
  dplyr::select(-term) %>% 
  mutate(era = "early")

alex %>% 
  filter(year %in% 2000:2010) %>% 
  group_by(region) %>% 
  do(tidy = tidy(lm(area_glacier_km2_hg ~ year, data = .))) %>% 
  unnest(tidy) %>% 
  filter(term == "year") %>% 
  dplyr::select(-term) %>% 
  mutate(era = "early")

mass_change %>%
  filter(include == "Y") %>% 
  dplyr::select(-include) %>% 
  pivot_longer(cols = c(-region,-area_km2)) %>% 
  mutate(era = str_split_fixed(name,"_",2)[,1],
         stat = str_split_fixed(name,"_",2)[,2]) %>% 
  dplyr::select(-name) %>% 
  pivot_wider(names_from = stat, values_from = value) %>% 
  mutate(dmdt = 0.850*area_km2*lm/1000,
         dmdt_err = 0.850*area_km2*se/1000)


%>% 
  ggplot() + 
    geom_point(aes(area_km2, lm)) + 
    facet_wrap(~era, scales = "free")

  

get_area_trends <- function(data, mass_change_data, region){
  
  reg = data[data['region'] == region,]
  
  e_slope <- lm(area_glacier_km2_hg~year, data = filter(reg, year %in% 2000:2010))
  l_slope <- lm(area_glacier_km2_hg~year, data = filter(reg, year %in% 2011:2020))
  f_slope <- lm(area_glacier_km2_hg~year, data = filter(reg, year %in% 2000:2020))
  
  tmp = mass_change[mass_change['region'] == region,] %>% filter(!is.na(region))
  
  e_dmdt     = pull(0.850 * tmp['area_km2']*tmp['early_lm']/1000)
  e_dmdt_err = pull(0.850 * tmp['area_km2']*tmp['early_se']/1000)
  
  l_dmdt     = pull(0.850 * tmp['area_km2']*tmp['late_lm']/1000)
  l_dmdt_err = pull(0.850 * tmp['area_km2']*tmp['late_se']/1000)
  
  f_dmdt     = pull(0.850 * tmp['area_km2']*tmp['full_lm']/1000)
  f_dmdt_err = pull(0.850 * tmp['area_km2']*tmp['full_se']/1000)

  
  as.data.frame(dmdt = c(e_dmdt, l_dmdt, f_dmdt),
                dmdt = c(e_dmdt, l_dmdt, f_dmdt))
  dmdadt = pd.DataFrame({'dmdt' : [e_dmdt, l_dmdt, f_dmdt], 'dmdt_err' : [e_dmdt_err, l_dmdt_err, f_dmdt_err],
    'dadt' : [e_slope, l_slope, f_slope],
    'dadt_err' : [e_std_err, l_std_err, f_std_err]}, dtype=np.float64)
  
dmdadt = pd.DataFrame({'dmdt' : [e_dmdt, l_dmdt, f_dmdt], 'dmdt_err' : [e_dmdt_err, l_dmdt_err, f_dmdt_err],
  'dadt' : [e_slope, l_slope, f_slope],
  

# full_join(df, gl) %>% 
#   # filter(region != "SCM") %>% 
#   filter(stat == "lm") %>% 
#   mutate(era = case_when(era == "early" ~ "2000-2010",
#                          era == "late" ~ "2010-2020",
#                          era == "full" ~ "2000-2020")) %>% 
#   mutate(era = factor(era, levels=c("2000-2010","2010-2020","2000-2020"))) %>% 
#   dplyr::select(-stat, -term) %>% 
#   rename(mass_loss_rate = val, area_loss_rate = estimate) %>% 
#   dplyr::select(era, region, mass_loss_rate, area_loss_rate, std.error, statistic, p.value) %>% 
#   write.csv("Code/3_analysis/Fig_MB/area_vs_hugonnet.csv")

  
  # full_join(df, gl) %>% 
  #   filter(stat == "lm") %>% 
  #   mutate(era = case_when(era == "early" ~ "2000-2010",
  #                          era == "late" ~ "2010-2020",
  #                          era == "full" ~ "2000-2020")) %>% 
  #   mutate(era = factor(era, levels=c("2000-2010","2010-2020","2000-2020"))) %>% 
  #   ggplot() + 
  #   geom_point(aes(estimate, val, color = region), show.legend = F) + 
  #   geom_smooth(aes(estimate, val), method = "lm", se = F, color = "grey", show.legend = F) +
  #   facet_wrap(~era, scales = "free") + 
  #   theme(aspect.ratio = 1) +
  #   ggrepel::geom_text_repel(aes(estimate, val, label = region, color = region), show.legend = F) + 
  #   labs(x = expression(Glacier~area~(km^2~a^-1)), 
  #        y = expression(Mass~balance~(kg~m^-2~a^-1))) +
  #   egg::theme_article() + 
  #   theme(aspect.ratio = 1)