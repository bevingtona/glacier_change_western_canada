library(broom)
library(tidyverse)


GLACIERS <- 
  bind_rows(
    read.csv("http://wgms.ch/data/min-data-series/FoG_MB_57.csv", skip = 13, sep = ";"),
    read.csv("http://wgms.ch/data/min-data-series/FoG_MB_45.csv", skip = 13, sep = ";"),
    read.csv("http://wgms.ch/data/min-data-series/FoG_MB_41.csv", skip = 13, sep = ";"))

GLACIERS <- GLACIERS %>% 
  as_tibble() %>% 
  select(NAME,SURVEY_YEAR,REFERENCE_YEAR,WINTER_BALANCE,SUMMER_BALANCE,ANNUAL_BALANCE) %>% 
  group_by(NAME) %>% 
  mutate(ANNUAL_BALANCE_CUM = cumsum(ANNUAL_BALANCE), 
       EPOCH = case_when(
                         SURVEY_YEAR <= 2011 ~ "early",
                         SURVEY_YEAR > 2011  ~ "late", 
                         TRUE ~ "b")) 

GLACIERS %>% 
  ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(aes(SURVEY_YEAR, ANNUAL_BALANCE, color = NAME)) +
  geom_smooth(aes(SURVEY_YEAR, ANNUAL_BALANCE, linetype = EPOCH, 
                  color = paste(NAME)), 
              method = "lm", se = F) + 
  theme_bw() + 
  scale_x_continuous(n.breaks = 10)


GLACIERS %>% 
  group_by(NAME, EPOCH) %>% 
  do(lm = tidy(lm(ANNUAL_BALANCE~SURVEY_YEAR, data = .))) %>% 
  unnest(lm) %>% 
  filter(term == "SURVEY_YEAR")
