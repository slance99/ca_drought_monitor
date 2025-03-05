library(here)
library(janitor)
library(tidyverse)
library(dplyr)

# cleaning for 2021
fire_data_2021 <- read_csv(here("data", "fire_data_raw.csv")) %>% 
  clean_names() %>% select(year, county_nam, gis_acres, fire_name) %>% 
  filter(year == 2021) %>% 
  rename(county = county_nam, acres_burn = gis_acres) %>%   
  drop_na(county) 


fire_data_mod_2021 <- fire_data_2021 %>% 
  group_by(county) %>%
  summarise(
    avg_acres_burned = mean(acres_burn, na.rm = TRUE),
    avg_fires_per_year = n() / n_distinct(year),
    total_acres_burned = sum(acres_burn, na.rm = TRUE),
    total_fires = n()
  ) %>%
  ungroup()

write_csv(fire_data_mod_2021, here("data", "fire_data_2021.csv"))


# old cleaning script
fire_data <- read_csv(here("data", "fire_data_raw.csv")) %>% 
  clean_names() %>% select(year, county_nam, gis_acres, fire_name) %>% 
  filter(year >= 2000 & year <= 2024) %>% 
  rename(county = county_nam, acres_burn = gis_acres) %>%   
  drop_na(county) 
 
fire_data_mod <- fire_data %>% 
  group_by(county, year) %>%
  summarise(
    avg_acres_burned = mean(acres_burn, na.rm = TRUE),
    avg_fires_per_year = n() / n_distinct(year),
    total_acres_burned = sum(acres_burn, na.rm = TRUE),
    total_fires = n()
  ) %>%
  ungroup()

fire_data_clean <- fire_data %>%
  left_join(fire_data_mod, by = c("county"))

write_csv(fire_data_clean, here("data", "fire_data_clean.csv"))



