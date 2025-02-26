#LOADING IN LIBRARIES
library(tidyverse)
library(dplyr)
library(janitor)
library(here)

#LOADING IN DATA
prism_og <- read_csv(here("data", "prism_climate.csv"))

#CLEANING DATA

#Creating Yearly Version with Cleaned Names + No NA for the Graphs in Climate
prism_month <- prism_og |>
  clean_names() |>
  drop_na() |>
  rename(county = name) |>
  pivot_longer(cols = -c(county, latitude, longitude, elevation_m, date),
               names_to = "climate_factor",
               values_to = "value") |>
  write_csv(here("data", "monthly_prism_climate.csv")) 
  

#Creating Annual Version for PCA
prism_annual <- prism_og |>
  clean_names() |>
  drop_na() |>
  rename(county = name) |>
  group_by(county) |>
  summarise(across(ppt_mm:vpdmax_h_pa, mean)) |>
  ungroup() |>
  write_csv(here("data", "annual_prism_climate.csv"))


