
df <- read.csv("Code/3_analysis/Fig_MB/menounos_et_al.csv") %>% 
  filter(include == "Y") %>% 
  select(-include,-area_km2, -starts_with("budget")) %>% 
  pivot_longer(cols = c(-region), names_to = "var", values_to = "val") %>% 
  mutate(era = str_split(var, "_", 2, simplify = T)[,1],
         stat = str_split(var, "_", 2, simplify = T)[,2]) %>% 
  select(-var)

gl <- bind_rows(
  new_all_join_id_filter %>% 
  st_drop_geometry() %>% 
  mutate(era = case_when(year %in% 2000:2009 ~ "early",
                         year %in% 2009:2018 ~ "late", 
                         TRUE ~ "N")) %>% 
  filter(era != "N") %>% 
  group_by(era, region, year) %>% 
  summarize(sum = sum(area_glacier_km2)) %>% 
  do(tidy = tidy(lm(sum ~ year, data = .))) %>% 
  unnest(tidy) %>% 
  filter(term == "year"),
  new_all_join_id_filter %>% 
  st_drop_geometry() %>% 
  mutate(era = case_when(year %in% 2000:2018 ~ "full",
                         TRUE ~ "N")) %>% 
  filter(era != "N") %>% 
  group_by(era, region, year) %>% 
  summarize(sum = sum(area_glacier_km2)) %>% 
  do(tidy = tidy(lm(sum ~ year, data = .))) %>% 
  unnest(tidy) %>% 
  filter(term == "year"))

full_join(df, gl) %>% 
  filter(stat == "lm") %>% 
  mutate(era = case_when(era == "early" ~ "2000-2009",
                         era == "late" ~ "2009-2018",
                         era == "full" ~ "2000-2018")) %>% 
  mutate(era = factor(era, levels=c("2000-2009","2009-2018","2000-2018"))) %>% 
  ggplot() + 
  geom_point(aes(estimate, val, color = region), show.legend = F) + 
  geom_smooth(aes(estimate, val), method = "lm", se = F, color = "grey", show.legend = F) +
  facet_wrap(~era, scales = "free") + 
  theme(aspect.ratio = 1) +
  ggrepel::geom_text_repel(aes(estimate, val, label = region, color = region), show.legend = F) + 
  labs(x = expression(Glacier~area~(km^2~a^-1)), 
       y = expression(Mass~balance~(kg~m^-2~a^-1))) +
  egg::theme_article() + 
  theme(aspect.ratio = 1)













df <- read.csv("Code/3_analysis/Fig_MB/menounos_et_al.csv") %>% 
  filter(include == "Y") %>% 
  select(region, starts_with("budget")) %>% 
  pivot_longer(cols = c(-region), names_to = "var", values_to = "val") %>% 
  mutate(era = str_split(var, "_", 2, simplify = T)[,1],
         stat = str_split(var, "_", 2, simplify = T)[,2]) %>% 
  select(-var) %>% 
  mutate(era = sub("budget","full",era))

gl <- new_all_join_id_filter %>% 
    st_drop_geometry() %>% 
    mutate(era = case_when(year %in% 2000:2018 ~ "full",
                           TRUE ~ "N")) %>% 
    filter(era != "N") %>% 
    group_by(era, region, year) %>% 
    summarize(sum = sum(area_glacier_km2)) %>% 
    do(tidy = tidy(lm(sum ~ year, data = .))) %>% 
    unnest(tidy) %>% 
    filter(term == "year")

full_join(df, gl) %>% 
  filter(stat == "lm") %>% 
  mutate(era = case_when(era == "early" ~ "2000-2009",
                         era == "late" ~ "2009-2018",
                         era == "full" ~ "2000-2018")) %>% 
  mutate(era = factor(era, levels=c("2000-2009","2009-2018","2000-2018"))) %>% 
  ggplot() + 
  geom_point(aes(estimate, val, color = region), show.legend = F) + 
  geom_smooth(aes(estimate, val), method = "lm", se = F, color = "grey", show.legend = F) +
  facet_wrap(~era, scales = "free") + 
  theme(aspect.ratio = 1) +
  ggrepel::geom_text_repel(aes(estimate, val, label = region, color = region), show.legend = F) + 
  labs(x = expression(Glacier~area~(km^2~a^-1)), 
       y = expression(Mass~budget~(Gt~a^-1))) +
  egg::theme_article() + 
  theme(aspect.ratio = 1)
