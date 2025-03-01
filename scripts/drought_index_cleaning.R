library(here)
library(tidyverse)
library(dplyr)
library(janitor)

drought_index <- read_csv(here("data", "county_drought_index_2021.csv")) %>% 
  clean_names() %>% 
  mutate(usdm_numeric = as.numeric(gsub("D", "", usdm_level)))

county_drought <- drought_index %>% 
  group_by(name) %>%
  summarise(
    mean_usdm = round(mean(usdm_numeric, na.rm = TRUE)),
    median_usdm = median(usdm_numeric, na.rm = TRUE),
    max_usdm = max(usdm_numeric, na.rm = TRUE),
    severe_drought_prop = mean(usdm_numeric >= 3, na.rm = TRUE),
    sd_usdm = sd(usdm_numeric, na.rm = TRUE)
  )
