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
      
      .regular-hover {
    border-radius: 0px; /* Rounds the edges of the image */
    transition: transform 0.2s ease, box-shadow 0.2s ease; /* Smooth hover effect */
    }
    
    .regular-hover:hover {
    transform: scale(1.015); /* Slightly enlarge the image */
    box-shadow: 0 8px 16px rgba(0, 0, 0, 0.2); /* Add a shadow */
    }
    
    .slider-animate-button {
    font-size: 30px !important;  /* Increase text size */
    padding: 10px 20px !important; /* Increase button size */
    border-radius: 5px !important; /* Optional: Round the corners */
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
               tags$figcaption(tags$i("Low Water Levels in the Oroville Dam in California. Photo by Noah Berger.")),
               
               # First row: text panel on the left, image on the right
               fluidRow(
                 column(6,
                        h3("Understanding Drought Risk in California"),
                        p("With its dry Mediterranean climate, California is particularly vulnerable to drought, 
                        which has become more frequent and severe as a result of climate change. 
                        Although droughts are a natural part of the climate system, 
                        their intensity and duration have been amplified by rising temperatures and 
                        shifting precipitation patterns. These changes have far-reaching effects on water supply, 
                        agriculture, and ecosystems. Prolonged droughts can lead to significant crop losses, 
                        reduced water availability, and negative impacts on agricultural economies and livelihoods. 
                        Additionally, drought contributes to increased wildfire risk by reducing fuel moisture, 
                        which makes landscapes more susceptible to catastrophic fires. Recent extreme droughts, 
                        such as the five-year period from 2012 to 2016, underscore the urgent need for effective 
                        drought management and climate adaptation strategies."),
                        br(),
                        p("Effective drought management at the statewide level requires comprehensive monitoring to 
                          guide timely and appropriate intervention strategies. State and federal agencies, including 
                          the California Department of Water Resources, USDA, NOAA, and the National Integrated Drought 
                          Information System, play key roles in monitoring and forecasting drought conditions, developing 
                          water management plans, and supporting affected communities."),
                        br(),
                        p("Drought dynamics are often informed by the following environmental variables: RILEY INSERT CONTENT HERE")
                 ),
                 column(6,
                        tags$img(src = "almond_drought.jpg", 
                                 alt = "An abandoned almond orchard in Newman, California impacted by drought", 
                                 class = "regular-hover",
                                 style = "width: 100%; height: 400px;"),
                        tags$figcaption(tags$i("Abandoned drought-stricken almond orchard in Newman, California. Photo by Terry Chea, AP."))
               )
               ),
               
               br(),  # This will add space between rows
               br(),  # This will add space between rows
               br(),  # This will add space between rows
               
               # Second row: image on the left, text panel on the right
               fluidRow(
                 column(6,
                        tags$img(src = "IMG_1982.jpeg", 
                                 alt = "Photo of Fire on Hillside During a Prescribed Burn", 
                                 class = "regular-hover",
                                 style = "width: 100%; height: 400px;"),
                        tags$figcaption(tags$i("Prescribed Fire for Managing Fuels and Wildfire Risk at Sedgwick Reserve in Santa Ynez, California.
                                        Photo by Thuy-Tien Bui."))
                 ),
                 column(6,
                        h3("Navigating the Website"),
                        p("This Shiny App offers an interactive platform to visualize spatial variations in drought severity, analyze patterns in drought parameters, 
                        and explore the distribution of drought-related environmental justice impacts across different counties.

                          Each tab provides the following:"),
                        HTML("<strong>Background</strong>"),  # HTML to make text bold
                        p("Introduction to the project, explantion of the importance of understanding drought and its
                          predictors in California, guide for navigating the website."),
                        HTML("<strong>Drought Map</strong>"),  # HTML to make text bold
                        p("Interactive map of changes in drought severity throughout California 
                          between 2000 and 2024."),
                        HTML("<strong>Principal Component Analysis</strong>"),  # HTML to make text bold
                        p("Biplot produced by a Principal Component Analysis (a statistical analysis to understand how specific 
                          variables are correlated with one another) to understand the relationships between 
                          different climate variables and drought for California counties."),
                        HTML("<strong>Climate Trends</strong>"),  # HTML to make text bold
                        p("Line graphs of different climate variables over time for California counties."),
                        HTML("<strong>Environmental Justice</strong>"),  # HTML to make text bold
                        p("Boxplots of environmental justice metrics for El Dorado and Los Angeles counties.")
                        
                 )
               )
      ),
      
      #############################################
      
      # DROUGHT MAP Tab
      tabPanel("Drought Map", 
               h3("Map of Drought Conditions from 2000 to 2024"),
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
                        leafletOutput("map", height = "100vh", width = "100vh")
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
                             This PCA focuses on climate variables and their relationship to drought conditions within California Counties in 2021.
                             
                             <br><br><b><span style='color: #E95420;'>To understand the relationships between each
                             of these variables, select at least two variables using the checkboxes below:</span></b><br><br>")),
                        
                        wellPanel(checkboxGroupInput("pca_variables",
                                                     label = "Climate Variables",
                                                     choices = NULL)
                        )
                 ),
                 column(7,
                        plotOutput("biplot", height = "100vh", width = "90%")
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
               fluidRow(
                 p("The data used in this Shiny app was sourced from the following datasets:"),
                        DTOutput("data_source")),
               br(),
               br(),
               fluidRow(
                 h3("Acknowledgements and Attributions"),
                 p("This website was created as a part of ESM 244 - Advanced Data Analysis at the Bren
                   School at UCSB taught by Nathan Grimes"),
                 p("Thuy-Tien Bui, Riley Black, and Sam Lance created the site collaboratively, each creating
                   their own tab. Thuy-Tien created the Drought Map and created + executed the theming of the site, 
                   Riley Black created the PCA and EJ tabs, and Sam Lance created the Climate Trends and Background tabs.")
               )
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
  data <- joined_drought_data() %>% 
    select(-date)
  
  # Get only numeric columns
  numeric_columns <- colnames(data)[sapply(data, is.numeric)]
  
  # Select variables to start
  selected_columns <- c("Drought Index", "Total Acres Burned", "Total Precipitation (mm)", "Average Temperature C")
  
  # Update the checkbox choices to include only numeric columns
  updateCheckboxGroupInput(session, "pca_variables", choices = numeric_columns, selected = selected_columns)
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


