#LOADING IN LIBRARIES
library(tidyverse)
library(dplyr)
library(janitor)
library(here)

# read filtered ces4 data
filtered_ces4 <- read_csv(here("data", "filtered_ces4.csv"))

filtered_ces4 <- filtered_ces4 |>
  rename(county=california_county) |>
  select(county,
         total_population,
         poverty,
         asthma,
         pm2_5,
         cardiovascular_disease,
         drinking_water,
         imp_water_bodies,
         groundwater_threats,
         pollution_burden_score,
         ozone
  ) 

# pivot longer for shiny app
ces4_longer <- filtered_ces4 |>
  pivot_longer(cols = -county,
               names_to = "ej_variable",
               values_to = "value")

write_csv(ces4_longer, here("data","ces4_longer.csv"))




