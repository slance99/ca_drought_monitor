library(shiny)
library(leaflet)
library(sf)
library(shinyWidgets)
library(tidyverse)
library(dplyr)
library(here)
library(ggplot2)
library(janitor)
library(readr)
library(DT)
library(viridis)
library(shinythemes)
library(ggfortify)

############################################################################
############################################################################
############################################################################

#############################################

########### DROUGHT MAP - TB ###########

#############################################

data_dir <- here("data", "drought_index")

shp_files <- list.files(data_dir, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)

california_shp <- here("data", "ca_state")

ca_boundary <- st_read(california_shp) %>% 
  st_transform(4326)

shp_data <- data.frame(
  file_path = shp_files,
  year = as.numeric(substr(gsub(".*USDM_(\\d{8})\\.shp", "\\1", basename(shp_files)), 1, 4))
) %>% 
  arrange(year) %>% 
  distinct(year, .keep_all = TRUE)

#############################################

########### CLIMATE TRENDS - SL ###########

#############################################

climate_data <- read_csv(here("data", "monthly_prism_climate.csv")) %>%
  mutate(
    climate_factor = recode(climate_factor, 
                            ppt_mm = "Precipitation (mm)",
                            tmax_degrees_c = "Max Temperature (°C)",
                            tmean_degrees_c = "Mean Temperature (°C)",
                            tmin_degrees_c = "Minimum Temperature (°C)",
                            vpdmin_h_pa = "Minimum Vapor Pressure Deficit (hPa)",
                            vpdmax_h_pa = "Maximum Vapor Pressure Deficit (hPa)")) |>
  mutate(county = case_when(
    county == "Santa Barbar" ~ "Santa Barbara",
    county == "San Francisc" ~ "San Francisco",
    county == "San Bernardi" ~ "San Bernardino",
    county == "San Luis Obi" ~ "San Luis Obispo",
    TRUE ~ county)) |>
  filter(!(county %in% c("Del Norte", "Modoc", "Mono", "Alpine"))) # these counties dont match up in each dataset

climate_data <- climate_data |>
  mutate(county = paste0(climate_data$county, " County"))

#############################################

########### PCA - RB ###########

#############################################

joined_drought_data <- read_csv(here("data","joined_drought_data.csv"))


#############################################

########### CITATIONS - TB ###########

#############################################

data_source <- read_csv(here("data","data_citations.csv"))

data_source$`Source` <- paste0(
  '<a href="', data_source$`Link`, '" target="_blank">',
  data_source$`Source`, '</a>'
)

data_source <- data_source[, !names(data_source) %in% "Link"]

############################################################################
############################################################################
############################################################################

ui <- fluidPage(
  theme = shinytheme("united"),
  
  tags$head(
    tags$style(HTML("

      .irs-bar {
        background: #E95420 !important;
        border-top: 1px solid #D43F00 !important;
        border-bottom: 1px solid #D43F00 !important;
      }

      .irs-bar-edge {
        background: #E95420 !important;
        border: 1px solid #D43F00 !important;
      }
      
      
       /* Style for the play button */
      .irs-slider-animate-btn {
        font-size: 30px !important; /* Adjust the font size */
        transform: scale(1.5) !important; /* Scale the button */
        width: 50px !important; /* Adjust the width */
        height: 50px !important; /* Adjust the height */
        line-height: 50px !important; /* Center the text vertically */
      }

      .irs-slider {
        background: #D43F00 !important;
        border: 1px solid #D43F00 !important;
      }

      .irs-grid-text {
        font-size: 12px !important;
        color: #555 !important;
      }

      .irs-single {
        background: #E95420 !important;
        color: white !important;
        border: 1px solid #D43F00 !important;
      }

      /* Style for the checkbox input itself */
      div.shiny-input-checkbox-group input[type='checkbox'] {
        border: 2px solid #E95420 !important;  /* Orange border */
        background-color: #fff !important;     /* Default white background */
      }

      /* Style for checked checkboxes */
      div.shiny-input-checkbox-group input[type='checkbox']:checked {
        background-color: #E95420 !important;  /* Orange background when checked */
        border-color: #E95420 !important;      /* Orange border when checked */
      }
      
      /* Optional: Change the background color when selecting an item */
      
      .selectize-dropdown .active {
        background-color: #E95420 !important;
        color: white !important;
      }
      
      /* Make sure checkbox borders are orange */
      .checkbox input[type='checkbox'] {
        width: 15px;
        height: 15px;
        border: 2px solid #E95420 !important;
        background-color: white !important;
        appearance: none; /* Removes default checkbox styles */
        -webkit-appearance: none;
        -moz-appearance: none;
        border-radius: 3px; /* Optional: Rounds the corners */
        cursor: pointer;
        position: relative;
      }
      
      /* When the checkbox is checked, change background color */
      .checkbox input[type='checkbox']:checked {
        background-color: #E95420 !important;
        border-color: #E95420 !important;
        position: relative;
      }
      
      /* Adding a checkmark when checked */
      .checkbox input[type='checkbox']:checked::after {
        content: '✓'; /* Unicode checkmark */
        font-size: 11px;
        color: white !important;
        position: absolute;
        top: 0px;
        left: 1px;
        right: 1px;
        bottom: 1px;
      }
      
      /* Style checkbox labels */
      .checkbox label,
        div.checkbox label,
        div.shiny-input-checkbox-group label {
          color: #E95420 !important;
          font-weight: bold;
        }

      /* Style the labels */
      div.checkbox label {
        color: #E95420 !important; /* Orange text */
      }

      /* Dropdown styles */
      select {
        background-color: white !important; /* Set the dropdown background to white */
        color: #E95420 !important; /* Set text color to orange */
        border: 1px solid #D43F00 !important; /* Orange border */
        font-size: 14px !important;
      }

      select:focus {
        background-color: white !important; /* Keep the background white when focused */
        border: 1px solid #E95420 !important; /* Change the border to the orange highlight */
        color: #E95420 !important; /* Keep the text color as orange */
      }

      /* Optional: Add styles to dropdown list items */
      .selectize-dropdown, .selectize-input {
        background-color: white !important; /* Dropdown list background stays white */
        color: #E95420 !important; /* Dropdown list text color is orange */
        border: 1px solid #D43F00 !important; /* Orange border for the dropdown list */
      }

      .selectize-dropdown .item {
        color: #E95420 !important; /* Dropdown items have orange text */
      }

      /* Hover effect for dropdown items */
      .selectize-dropdown .item:hover {
        background-color: #E95420 !important; /* Orange background on hover */
        color: white !important; /* White text on hover */
      }

      /* Optional: Change the background color when selecting an item */
      .selectize-dropdown .active {
        background-color: #E95420 !important;
        color: white !important;
      }
      
    "))
  ),
  
  titlePanel("California Drought Explorer"),
  
  #############################################
  
  ########### DROUGHT INTRO - SL ###########
  
  #############################################
    
    tabsetPanel(
      id = "tabs",
      
      # Background Tab
      tabPanel("Background",
               # Top full-width image
               tags$img(src = "lake_oroville_drought.jpg", 
                        alt = "Image of Lake Oroville Dam with the Treeline High Above the Water Level Illustrating Losses from Evaporation and Use", 
                        style = "width: 100%; height: 400px;"),
               tags$figcaption("Low Water Levels in the Oroville Dam in California. Photo by Noah Berger."),
               
               # First row: text panel on the left, image on the right
               fluidRow(
                 column(6,
                        h3("Understanding Drought Risk in California"),
                        p("With its dry Mediterranean climate, California is particularly vulnerable to drought, 
                        which has become more frequent and severe due to the effects of climate change. 
                        While droughts are a natural part of the climate system, their intensity and 
                        duration are exacerbated by rising temperatures and shifting precipitation patterns, 
                        leading to serious impacts on water supply, agriculture, and ecosystems. 
                        Recent droughts, such as the five-year drought from 2012 to 2016, have 
                        highlighted the urgent need for effective drought management 
                        and climate adaptation strategies.")
                 ),
                 column(6,
                        tags$img(src = "dry_ranch.jpg", 
                                 alt = "A Dry Ranch with Cracked Soil in Fresno California During a Drought Event in 2014", 
                                 style = "width: 100%; height: 400px;"),
                        tags$figcaption("Fresno Ranch During Drought Event in 2014, U.S. Department of Agriculture Cynthia Mendoza/Videographer/USDA photo by Cynthia Mendoza, Public domain, via Wikimedia Commons.")
                 )
               ),
               
               # Second row: image on the left, text panel on the right
               fluidRow(
                 column(6,
                        tags$img(src = "another_image.jpg", 
                                 alt = "Additional Image", 
                                 style = "width: 100%; height: 400px;")
                 ),
                 column(6,
                        p("This Shiny app offers an interactive exploration of drought parameters across California.")
                 )
               )
      ),
      
      #############################################
      
      # DROUGHT MAP Tab
      tabPanel("Drought Map", 
               h3("How has the distribution of drought conditions changed over time?"),
               fluidRow(
                 column(5,
                        p(HTML("The U.S. Drought Monitor (USDM) has mapped drought conditions across the United States since 2000, providing real-time snapshots of drought severity.
                           Spatial drought monitoring is useful for decision-making in areas like water management, agriculture, and emergency response.
                           The USDM integrates multiple indicators, including precipitation, streamflow, reservoir levels, temperature, evaporative demand, soil moisture, and vegetation health.
                           The data in this map represents annual drought conditions during the peak drought season in late August. 
                           <br><br><b><span style='color: #E95420;'>Use the time slider below or click the play button to explore how drought conditions have evolved over time.</span></b><br><br>")),
                        sliderInput("year", "Select Year:",
                                    min = min(shp_data$year), 
                                    max = max(shp_data$year), 
                                    value = min(shp_data$year), 
                                    step = 1,
                                    sep = "",
                                    width = "100%",
                                    animate = animationOptions(interval = 1500, loop = TRUE)),
                        h4("Drought Index Categories"),
                        DTOutput("drought_table")
                 ),
                 column(6, 
                        leafletOutput("map", height = "100vh")
                 )
               )
      ),
      
      #############################################
      
      # Principal Component Analysis (PCA) Tab
      tabPanel("Principal Component Analysis", 
               h3("Principal Component Analysis for Environmental Variables Related to Drought"),
               fluidRow(
                 column(5,
                        p(HTML("Principal Component Analysis (PCA) is an unsupervised machine learning ordination method, 
                             or linear dimensionality reduction. PCA projects a swarm of multi-dimensional data
                             onto a two dimensional plot with Principal Components (PC) on each axis chosen based on 
                             the direction of the data with the greatest variance. PCA is useful for multidimensional data exploration
                             and can tell us a lot about correlations between many variables within a dataset. 
                             This PCA focuses on climate variables and their relationship to drought conditions within California Counties in 2021
                             
                             <br><br><b><span style='color: #E95420;'>To understand the relationships between each
                             of these variables, select at least two variables using the checkboxes below</span></b><br><br>")),
                        
                        wellPanel(checkboxGroupInput("pca_variables",
                                                     label = "Climate Variables",
                                                     choices = NULL)
                        )
                 ),
                 column(7,
                        plotOutput("biplot", height = "500px", width = "90%")
                 )
               )
      ),
      
      #############################################
      
      # Climate Factors Tab
      tabPanel("Climate Trends", 
               h3("Understanding Climate Trends for California Counties"),
               fluidRow(
                 column(5,
                        p(HTML("Climate factors such as precipitation, temperature, and vapor pressure deficit play a major role in 
                             determining drought conditions. Increased temperature and decreased precipitation can lead to more severe and prolonged droughts.
                             Due to climate change, these factors have and will continue to experience significant changes over time, impacting drought risk.
                             
                             <br><br><b><span style='color: #E95420;'> To see how these factors have changed for individual counties in California, select a county and climate factor of interest. </span></b><br><br>")),
                        wellPanel(selectInput("county_cl",
                                              label = "Select County",
                                              choices = NULL),
                                  selectInput("climate_factor",
                                              label = "Select Climate Variable",
                                              choices = NULL)
                        )
                 ),
                 column(7,
                        plotOutput("climate_plot", height = "500px", width = "90%")
                 )
               )
      ),
      
      #############################################
      
      # Environmental Justice Tab
      tabPanel("Environmental Justice", 
               h3("Health and Human Impacts Related to Drought"),
               fluidRow(
                 column(5,
                        p(HTML("Water security, quality, wildfires, air quality, and other issues can all be caused or exacerbated by drought.
                             Marginalized groups and those with the least resources often bear the brunt of impacts from drought. Environmental justice issues
                             vary widely based on region and county size, however, due to differences in population density and resources. Two counties that exemplify this 
                             difference would be Los Angeles, a large urban city in Southern California, and El Dorado, a small rural county in Northern California. 
                             
                             <br><br><b><span style='color: #E95420;'> To explore how different Environmental Justice metrics
                             differ between the two counties, select your desired variable below </span></b><br><br>")),
                        
                        wellPanel(selectInput("ej_variable",
                                              label = "Select Environmental Justice Metric",
                                              choices = NULL)),
                        tableOutput("meta_data")
                 ),
                 column(6,
                        offset = 1,
                        plotOutput("ej_box_plot", height = "500px", width = "90%"),
                        tags$figcaption("Health and human impacts related to drought across El Dorado and Los Angeles County Census Tracts.
                                      According to CES4 (2021), El Dorado has 42 census tracts and Los Angeles has 2343 census tracts.")
                 )
               )
      ),
      
      #############################################
      
      # Data & References Tab
      tabPanel("Data & References",
               h3("Data Sources"),
               p("The data used in this Shiny app was sourced from the following datasets:"),
               DTOutput("data_source")
      )
    )
  )

############################################################################
############################################################################
############################################################################

# Define server logic
server <- function(input, output, session) {
  
  ############################
  
  ###### DROUGHT MAP - TB ############
  
  ############################
  
  # More drought server parameters
  selected_shp <- reactive({
    req(input$year)
    
    file_to_load <- shp_data %>%
      filter(year == input$year) %>%
      pull(file_path)
    
    if (length(file_to_load) > 0) {
      drought_data <- st_read(file_to_load[1]) %>% 
        st_transform(4326) %>% 
        st_make_valid()  # Fix invalid geometries
      
      drought_ca <- st_intersection(drought_data, ca_boundary)  # Clip to California
      
      return(drought_ca)
    } else {
      return(NULL)
    }
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%  # Change to CartoDB Positron basemap
      setView(lng = -119.5, lat = 37, zoom = 6)  # Centered on California
  })
  
  observeEvent(input$tabs, {
    if (input$tabs == "Drought Map") {
      # Reset the map view
      leafletProxy("map") %>%
        setView(lng = -119.5, lat = 37, zoom = 6)  # Reset to California
      
      # Reset the time slider
      updateSliderInput(session, "year", value = min(shp_data$year))  # Reset to the minimum year value
    }
  })
  
  observe({
    req(selected_shp())
    
    # Define updated color palette for DM values
    pal <- colorFactor(
      palette = c("#FFFF00", "#FBD47F", "#FFAA01", "#E60001", "#710001"), 
      domain = c(0, 1, 2, 3, 4)
    )
    
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(data = selected_shp(), 
                  fillColor = ~pal(DM),  # Assign color based on DM values
                  color = NA,
                  weight = 0, 
                  fillOpacity = 1,
                  popup = ~paste("Drought Index:", DM))  # Add popup info
  })
  
  drought_index_data <- data.frame(
    Category = c("D0", "D1", "D2", 
                 "D3", "D4"),
    Description = c("Abnormally Dry", 
                    "Moderate Drought", 
                    "Severe Drought", 
                    "Extreme Drought", 
                    "Exceptional Drought"),
    Impacts = c("Going into drought: short-term dryness slows growth of crops/pastures. Coming out of drought: 
                       some lingering water deficits; crops/pastures not fully recovered.", 
                "Some damage to crops/pastures; streams, reservoirs, or wells are low with some water shortages developing or imminent; voluntary water-use restrictions requested",
                "Crop and pasture losses likely; water shortages are common and water restrictions are imposed",
                "Major crop/pasture losses; widespread water shortages or restrictions",
                "Exceptional and widespread crop/pasture losses; shortages of waster in reservoirs, streams, and wells creating water emergencies")
  )
  
  category_colors <- c("D0" = "#FFFF00", "D1" = "#FBD47F", "D2" = "#FFAA01", "D3" = "#E60001", "D4" = "#710001")
  
  output$drought_table <- renderDT({
    datatable(drought_index_data, 
              options = list(
                dom = 't',
                paging = FALSE,  
                searching = FALSE,
                columnDefs = list(list(
                  targets = 0,             
                  visible = FALSE          
                ))
              ),
              escape = FALSE) %>%
      formatStyle(
        "Category",
        fontWeight = 'bold',
        backgroundColor = styleEqual(names(category_colors), category_colors),
        color = styleEqual(
          c('D0', 'D1', 'D2', 'D3', 'D4'), 
          c('black', 'black', 'black', 'black', '#F5F5F5'))
      )
  })
  
  ############################
  
  ###### CLIMATE FACTORS - SL ############
  
  ############################
  
  observe({
    counties <- unique(climate_data$county)
    updateSelectInput(session, "county_cl", choices = counties)
  })
  
  # Dynamically update the climate factor choices from the 'climate factor' column
  observe({
    factors <- unique(climate_data$`climate_factor`)
    updateSelectInput(session, "climate_factor", choices = factors)
  })
  
  # Filter data based on selected county and climate factor
  filtered_data <- reactive({
    req(input$county_cl, input$climate_factor)  # wait for both inputs
    data <- climate_data %>%
      filter(county == input$county_cl, `climate_factor` == input$climate_factor)
    data
  })
  
  # Create the plot with year on the x-axis and the value on the y-axis
  output$climate_plot <- renderPlot({
    req(input$climate_factor)
    data <- filtered_data() 
    
    climate_palette <- c("Precipitation (mm)" = "#03045e", 
                        "Max Temperature (°C)" = "#c1121f",
                        "Mean Temperature (°C)" = "#fb8500",
                        "Minimum Temperature (°C)" = "#ffb703",
                        "Minimum Vapor Pressure Deficit (hPa)" = "#8ecae6",
                        "Maximum Vapor Pressure Deficit (hPa)" = "#219ebc")
    
    ggplot(data, aes(x = date, y = value, color = climate_factor)) + 
      geom_line(size = 1.2) +  # Slightly thicker line for better visibility
      geom_point(size = 3) +  # Larger points for better visibility)
      theme_minimal() +
      scale_color_manual(values = climate_palette) +  # Apply the custom color palette
      labs(x = "Year", 
           y = paste(input$climate_factor), 
           title = paste(input$climate_factor, "Trend in", input$county_cl)) +
      theme(
        axis.text.x = element_text(hjust = 1, size = 14),  # Larger x-axis labels
        axis.text.y = element_text(size = 14),  # Larger y-axis labels
        axis.title.x = element_text(size = 16),  # Larger x-axis title
        axis.title.y = element_text(size = 16),  # Larger y-axis title
        plot.title = element_text(size = 18, hjust = 0.5),  # Larger title
        legend.title = element_text(size = 14),  # Larger legend title
        legend.position = "none",
        legend.text = element_text(size = 12) # Larger legend text
      )
  })
  
  ############################

  ###### ENVIRONMENTAL JUSTICE - RB ############
  
  ############################

# Load the data reactively
ces4_longer <- reactive({
  read_csv(here("data","ces4_longer.csv"))
})

meta_data <- reactive({
  read_csv(here("data","meta_data.csv"))
})

# Dynamically update the EJ factor choices from the 'ej_variable' column
observe({
  data <- ces4_longer()
  factors <- unique(data$`ej_variable`)
  updateSelectInput(session, "ej_variable", choices = factors)
})

# Filter data based ej variable
filtered_ej_data <- reactive({
  req(input$ej_variable)
  data <- ces4_longer() %>%
    filter(`ej_variable` == input$ej_variable)
  data
})

# Create the plot
output$ej_box_plot <- renderPlot({
  req(input$ej_variable)
  data <- filtered_ej_data()
  
  ggplot(data, aes(x = county, y = value, fill = county)) + 
    geom_boxplot(alpha = 0.8) +
    theme_minimal() +
    labs(x = "County", 
         y = paste(input$ej_variable), 
         title = paste("Differences in", input$ej_variable, "\nBetween El Dorado and Los Angeles Counties")) +
    scale_fill_manual(values = c("grey", "#E95420")) +  
    theme(
      axis.text.x = element_text(hjust = 1, size = 14),  # Larger x-axis labels
      axis.text.y = element_text(size = 14),  # Larger y-axis labels
      axis.title.x = element_text(size = 16),  # Larger x-axis title
      axis.title.y = element_text(size = 16),  # Larger y-axis title
      plot.title = element_text(size = 16, hjust = 0.5),  # Larger title
      legend.position = "none",
      legend.text = element_text(size = 12) # Larger legend text
    ) 
})

# Render the table
output$meta_data <- renderTable({
  meta_data()
})

############################

###### PCA - RB ############

############################

# Load the data reactively
joined_drought_data <- reactive({
  read_csv(here("data", "joined_drought_data.csv"))
})

# Dynamically update the checkbox choices from the numeric columns of the data
observe({
  data <- joined_drought_data() |>
    select(-date)
  
  # Get only numeric columns
  numeric_columns <- colnames(data)[sapply(data, is.numeric)]
  
  # Update the checkbox choices to include only numeric columns
  updateCheckboxGroupInput(session, "pca_variables", choices = numeric_columns, selected = numeric_columns)
})

# Filter the data based on selected variables for PCA
pca_data <- reactive({
  req(input$pca_variables)  # Ensure some variables are selected
  
  # Filter the data by selected variables
  data <- joined_drought_data()
  data_selected <- data[, input$pca_variables, drop = FALSE]
  
  # Remove any columns that have zero variance
  data_selected <- data_selected[, apply(data_selected, 2, var) != 0]
  
})

# Perform PCA on the selected variables
pca_result <- reactive({
  req(pca_data())  # Ensure the filtered data is ready
  prcomp(pca_data(), scale = TRUE)
})

# Define custom colors in a vector
color_values <- c("#FFFF00", "#FBD47F", "#FFAA01", "#E60001", "#710001")
color_palette <- setNames(color_values, c("D0", "D1", "D2", "D3", "D4"))

# Plot the PCA biplot
output$biplot <- renderPlot({
  req(pca_result())
  pca <- pca_result()
  
  autoplot(pca, 
           data = joined_drought_data(), 
           loadings = TRUE, 
           color = "USDM_index",
           loadings.label = TRUE,
           loadings.colour = "black",
           loadings.label.colour = "black",
           loadings.label.size = 5) +
    scale_color_manual(values = color_palette) +
    theme_minimal() +
    labs(title = "PCA of Selected Drought and Climate Conditions") +
    theme(
      axis.text.x = element_text(size = 14),  # Larger x-axis labels
      axis.text.y = element_text(size = 14),  # Larger y-axis labels
      axis.title.x = element_text(size = 16),  # Larger x-axis title
      axis.title.y = element_text(size = 16),  # Larger y-axis title
      plot.title = element_text(size = 18, hjust = 0.5),# Larger title
      legend.position = "none") 
    
})

### Scree Plots ###

# Create a dataframe with the necessary ingredients to make a scree plot
# output$screeplot <- renderPlot({
#   req(joined_drought_pca())  # Ensure PCA is ready
#   pca_result <- joined_drought_pca()  # Get PCA result
#   
#   pc_names <- colnames(pca_result$rotation)
#   sd_vec <- pca_result$sdev
#   var_vec <- sd_vec^2  # sd = variance^2
#   
#   pct_expl_df <- data.frame(v = var_vec,
#                             pct_v = var_vec / sum(var_vec),
#                             pc = pc_names)
  
#   # Screeplot
#   ggplot(pct_expl_df, aes(x = fct_reorder(pc, v, .desc = TRUE), y = v)) +
#     geom_col(fill = "steelblue") +
#     geom_text(aes(label = scales::percent(round(pct_v, 3))), vjust = 0, nudge_y = .5, angle = 90) +
#     labs(title = "Scree Plot of Principle Components", x = 'Principal component', y = 'Variance explained') +
#     theme_bw() +
#     theme(axis.text = element_text(size = 10)) +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
# })

output$data_source <- renderDT({
  datatable(data_source, 
            escape = FALSE,
            options = list(
              dom = 't',
              paging = FALSE,
              searching = FALSE, 
              columnDefs = list(list(
                targets = 0,            
                visible = FALSE         
              ))
            ))
})

}

shinyApp(ui = ui, server = server)


