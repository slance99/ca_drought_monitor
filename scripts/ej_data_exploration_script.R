### Setup
library(tidyverse)
library(here)
library(ggfortify) # For PCA biplot
library(lubridate)
library(dplyr)

#clear environment
#rm(list = ls())


### CalEnviroscreen 4.0 Data

ces4 <- read_csv("data/CES4.csv") |>
  janitor::clean_names() |>
  drop_na()

ces4 <- ces4 |>
  mutate(
    twocounty = case_when(
      california_county == "El Dorado" ~ "El Dorado",
      california_county == "Los Angeles" ~ "Los Angeles",
      TRUE ~ "Other County"
    )
  ) |>
  select(-ends_with("_pctl"))



#ces4_dem <- read_csv("data/CES4_demographic.csv") |>
  #janitor::clean_names()

### create df for just El Dorado and LA counties
ces4_eldor_la <- ces4 |>
  filter(california_county==c("El Dorado", "Los Angeles"))

### CES4 Data Exploration ###

ggplot(ces4_eldor_la, aes(x = asthma)) +
  geom_density(aes(fill=california_county), alpha = .5) 
#facet_wrap(~ california_county, scales = 'free_x')

ggplot(ces4_eldor_la, aes(x = pm2_5)) +
  geom_density(aes(fill=california_county), alpha = .5)


ggplot(ces4_eldor_la, aes(x = california_county, y = asthma)) +
  geom_col(aes(fill=california_county), alpha = .5) 


##############
### PCA 
##############

### Scaling### 

# prepare for PCA
ces4_pca <- ces4 |>
  drop_na() |>
  select(where(is.numeric))

# scale
ces4_pca <- ces4_pca |> 
  prcomp(scale = TRUE)

# See the loadings (weighting for each principal component)
ces4_pca$rotation


### Scree Plots ###

# create a dataframe with the necessary indgreidents to make a screeplot

pc_names <- colnames(ces4_pca$rotation)
sd_vec <- ces4_pca$sdev
var_vec <- sd_vec^2    # sd = variance^2


pct_expl_df <- data.frame(v = var_vec,
                          pct_v = var_vec / sum(var_vec),
                          pc = pc_names)

# Screeplot
scree1<-ggplot(pct_expl_df, aes(x = fct_reorder(pc, v, .desc = TRUE), y = pct_v)) +
  geom_col() +
  labs(x = 'Principal component', y = 'Variance explained')+
  scale_y_continuous(labels = scales::percent,expand = c(0,0))+
  theme_bw()+
  theme(axis.text = element_text(size=10)) +
  theme(axis.text.x = element_text(angle=45,hjust=1))

# Showing another way where we add the percentage explained as labels
scree2<-ggplot(pct_expl_df, aes(x = fct_reorder(pc, v, .desc = TRUE), y = v)) +
  geom_col(fill="steelblue") +
  geom_text(aes(label = scales::percent(round(pct_v,3))), vjust = 0, nudge_y = 1, angle=90) +
  labs(x = 'Principal component', y = 'Variance explained') +
  theme_bw()+
  theme(axis.text = element_text(size=10)) +
  theme(axis.text.x = element_text(angle=45,hjust=1))
scree2

# simple scree plot
screeplot(ces4_pca, type = "lines")


### PCA biplot ###
  

autoplot(ces4_pca,
         data = ces4,
         loadings = TRUE,
         colour = "twocounty",
         shape = "twocounty",
         alpha = case_when(ces4$twocounty == "Other County" ~ 0.2, TRUE ~ 1),
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



### Boxplots of EJ parameters

