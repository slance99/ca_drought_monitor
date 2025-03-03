#LOADING IN LIBRARIES
library(tidyverse)
library(dplyr)
library(janitor)
library(here)
library(ggfortify) # For PCA biplot


### CalEnviroscreen 4.0 data 
  # assuming 2021 (release date od CES4) for other environmental variables
ces4 <- read_csv(here("data","CES4.csv")) |>
  janitor::clean_names() |>
  drop_na() |>
  select(-ends_with(c("_pctl", "percentile")),
         -census_tract,
         -low_birth_weight,
         -latitude,
         -longitude,
         -zip) |>
  rename(county = california_county)


### Prism Climate Data
# climate_annual <- read_csv(here("data","annual_prism_climate.csv"))
prism_climate <- read_csv(here("data","prism_climate.csv"))

  # filter for 2021 to match CES4
climate_2021 <- prism_climate |>
  janitor::clean_names() |>
  drop_na() |>
  filter(date=="2021") |>
  rename(county = name)

climate_2021 <-climate_2021 |>
  mutate(county = case_when(
    county == "Santa Barbar" ~ "Santa Barbara",
    county == "San Francisc" ~ "San Francisco",
    county == "San Bernardi" ~ "San Bernardino",
    county == "San Luis Obi" ~ "San Luis Obispo",
    TRUE ~ county  # Keep all other counties as they are
  )) |>
  filter(!(county %in% c("Del Norte", "Modoc", "Mono", "Alpine"))) # these counties dont match up in each dataset

#######################

# join data by county
joined_data <- full_join(ces4, climate_2021, by = "county") |>
  #rename(county = california_county) |>
  select(-ces_4_0_percentile_range)

#joined_data <- climate_2021 |>
#  mutate(county = case_when(
 #   county == "Santa Barbar" ~ "Santa Barbara",
  #  county == "San Francisc" ~ "San Francisco",
   # county == "San Bernardi" ~ "San Bernardino",
    #county == "San Luis Obi" ~ "San Luis Obispo",
    #TRUE ~ county  # Keep all other counties as they are
  #)) |>
  #filter(county %in% c("Del Norte", "Modoc", "Mono", "Alpine"))


#######################
##############
### PCA 
##############

### Scaling### 

# prepare for PCA
joined_pca <- joined_data |>
  select(where(is.numeric)) |>
  drop_na() |>
  select_if(~ var(.x, na.rm = TRUE) != 0)  # Remove zero-variance columns

# scale
joined_pca <- joined_pca |> 
  prcomp(scale = TRUE)

# See the loadings (weighting for each principal component)
#joined_pca$rotationdim(joined_pca)



### Scree Plots ###

# create a dataframe with the necessary indgreidents to make a screeplot

pc_names <- colnames(joined_pca$rotation)
sd_vec <- joined_pca$sdev
var_vec <- sd_vec^2    # sd = variance^2


pct_expl_df <- data.frame(v = var_vec,
                          pct_v = var_vec / sum(var_vec),
                          pc = pc_names)

# Screeplot
joined_scree<-ggplot(pct_expl_df, aes(x = fct_reorder(pc, v, .desc = TRUE), y = pct_v)) +
  geom_col() +
  labs(x = 'Principal component', y = 'Variance explained')+
  scale_y_continuous(labels = scales::percent,expand = c(0,0))+
  theme_bw()+
  theme(axis.text = element_text(size=10)) +
  theme(axis.text.x = element_text(angle=45,hjust=1))

# Showing another way where we add the percentage explained as labels
joined_scree2<-ggplot(pct_expl_df, aes(x = fct_reorder(pc, v, .desc = TRUE), y = v)) +
  geom_col(fill="steelblue") +
  geom_text(aes(label = scales::percent(round(pct_v,3))), vjust = 0, nudge_y = 1, angle=90) +
  labs(x = 'Principal component', y = 'Variance explained') +
  theme_bw()+
  theme(axis.text = element_text(size=10)) +
  theme(axis.text.x = element_text(angle=45,hjust=1))
joined_scree2

# simple scree plot
screeplot(joined_pca, type = "lines")


### PCA biplot ###


autoplot(joined_pca,
         data = joined_data,
         loadings = TRUE,
         colour = "steelblue",
         alpha = 0.3,
         #shape = case_when(ces4$twocounty == "Other County" ~ 0.2, TRUE ~ 1),
         #alpha = case_when(ces4$twocounty == "Other County" ~ 0.2, TRUE ~ 1),
         loadings.label = TRUE,
         loadings.colour = "black",
         loadings.label.colour = "black",
         loadings.label.vjust = -0.5
) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  labs(
    colour = "County",
    shape = "County"
  )



