#LOADING IN LIBRARIES
library(tidyverse)
library(dplyr)
library(janitor)
library(here)
library(ggfortify) # For PCA biplot
library(DescTools)

### Drought Index Data
# 2021
drought_data <- read_csv(here("data","county_drought_index_2021.csv")) |>   ### no LA county?
  mutate(county = str_replace_all(Name, " County", ""))
  
### Mode funtction
# Define the get_mode function that always returns a single mode value
get_mode <- function(x) {
  # Calculate the frequencies of each unique value
  tabulated <- table(x)
  # Find the most frequent value(s)
  mode_val <- names(tabulated)[tabulated == max(tabulated)]
  # If there's a tie, return the first mode (alphabetically or numerically)
  return(mode_val[1])  
}

# Apply the mutation to calculate the mode of USDMLevelID by county
drought_index <- drought_data |>
  mutate(year = year(MapDate)) |>    # Create a 'year' column from 'MapDate'
  group_by(county) |>                # Group by county
  summarize(
    USDM_index = get_mode(USDMLevel),  # Calculate mode of USDMLevelID by county
    .groups = 'drop'  # To drop the grouping after summarizing
  )


### Prism Climate Data
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
joined_drought_data <- left_join(drought_index, climate_2021, by = c("county")) |>
  drop_na() 

joined_drought_data <- joined_drought_data %>%
  mutate(drought_index = case_when(
    USDM_index == "D0" ~ 0,
    USDM_index == "D1" ~ 1,
    USDM_index == "D2" ~ 2,
    USDM_index == "D3" ~ 3,
    USDM_index == "D4" ~ 4,
    TRUE ~ NA_real_
  ))


write_csv(joined_drought_data, here("data","joined_drought_data.csv"))

##############
### PCA 
##############

### Scaling### 

# prepare for PCA
joined_drought_pca <- joined_drought_data |>
  select(where(is.numeric)) |>
  drop_na() |>
  select_if(~ var(.x, na.rm = TRUE) != 0)  # Remove zero-variance columns

# scale
joined_drought_pca <- joined_drought_pca |> 
  prcomp(scale = TRUE)

# See the loadings (weighting for each principal component)
#joined_pca$rotationdim(joined_pca)



### Scree Plots ###

# create a dataframe with the necessary indgreidents to make a screeplot

pc_names <- colnames(joined_drought_pca$rotation)
sd_vec <- joined_drought_pca$sdev
var_vec <- sd_vec^2    # sd = variance^2


pct_expl_df <- data.frame(v = var_vec,
                          pct_v = var_vec / sum(var_vec),
                          pc = pc_names)

# Screeplot
drought_scree<-ggplot(pct_expl_df, aes(x = fct_reorder(pc, v, .desc = TRUE), y = pct_v)) +
  geom_col() +
  labs(x = 'Principal component', y = 'Variance explained')+
  scale_y_continuous(labels = scales::percent,expand = c(0,0))+
  theme_bw()+
  theme(axis.text = element_text(size=10)) +
  theme(axis.text.x = element_text(angle=45,hjust=1))

# Showing another way where we add the percentage explained as labels
drought_scree2<-ggplot(pct_expl_df, aes(x = fct_reorder(pc, v, .desc = TRUE), y = v)) +
  geom_col(fill="steelblue") +
  geom_text(aes(label = scales::percent(round(pct_v,3))), vjust = 0, nudge_y = 1, angle=90) +
  labs(x = 'Principal component', y = 'Variance explained') +
  theme_bw()+
  theme(axis.text = element_text(size=10)) +
  theme(axis.text.x = element_text(angle=45,hjust=1))
drought_scree2


### PCA biplot ###

# Define custom colors in a vector
color_values <- c("#FFFF00", "#FBD47F", "#FFAA01", "#E60001", "#710001")

# Create a named vector using setNames() for mapping
color_palette <- setNames(color_values, c("D0", "D1", "D2", "D3", "D4"))

autoplot(joined_drought_pca,
         data = joined_drought_data,
         loadings = TRUE,
         color = "USDM_index",
         loadings.label = TRUE,
         loadings.colour = "black",
         loadings.label.colour = "black",
         loadings.label.vjust = -0.5
) +
  scale_color_manual(values = color_palette) +  # Using the color_palette
  theme_minimal() +
  labs(
    colour = "Drought Index",
    shape = "County"
  )


#E95420
