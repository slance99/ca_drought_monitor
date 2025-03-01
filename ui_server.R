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

# Loading in drought shapefile data
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

# Define UI define
UI <- fluidPage(
  # Application title
  titlePanel("Understanding Drought Risk for Two California Counties"),
  
  ########### DROUGHT INTRO - SL ###########
  tabsetPanel(
    tabPanel("Background", 
             h3("Understanding Drought Risk in California"),
             tags$img(src = "la_photo.jpg", alt = "Sample Image", width = 1100, height = 400),
             p("Photo by DAVID ILIFF. License: CC BY-SA 3.0"),
             
             tags$br(), # Add a line break
             
             
             p("Due to its dry Mediterranean climate, California is particularly vulnerable to drought. 
               Droughts are a natural part of the climate system, but they can have serious impacts on water supply, agriculture, and ecosystems. 
               In recent years, California has experienced several severe droughts, including a five-year drought from 2012 to 2016. 
               Understanding the factors that contribute to drought risk is essential for effective water management and climate adaptation."),
             
             tags$br(), # Add a line break
             
             p("This Shiny app provides an interactive exploration of drought risk for two California counties: Los Angeles and El Dorado. 
               The app includes visualizations of climate trends, a principal component analysis of climate variables, and an environmental justice analysis. 
               Use the tabs above to navigate through the different sections of the app."),
             
             tags$br(), # Add a line break
             
             p("The [ INSERT DATA TYPE ] data was sourced from [ INSERT DATA SOURCE ] - repeat for all data as is input into the project")
    ),
    
    ############ DROUGHT MAP - TB ############
    tabPanel("Drought Map", 
             h3("How has the distribution of drought severity changed over time?"),
             p("[Use the slider to visualize drought conditions by date.]"),
             sliderInput("year", "Select Year:",
                         min = min(shp_data$year), 
                         max = max(shp_data$year),
                         value = min(shp_data$year), 
                         step = 1, 
                         animate = TRUE, 
                         sep = ""),
             leafletOutput("map", height = 600)  # Ensure it's inside the tabPanel
    ),
    
    ############ PCA - RB ############
    tabPanel("Principal Component Analysis", 
             h3("Principle Component Analysis for Environmental Variables Related to Drought"),
             p("Principle Component Analysis (PCA) is an unsupervised machine learning ordination method, 
               or linear dimensionality reduciton. PCA projects a swarm of mutlidimentional data
               onto a two dimentional plot with Principle Components (PC) on each axis chosen based on 
               the direction of the data with the greatest variance. PCA is useful for multidimentional data exploration
               and can tell us a lot about correlations between many variables within a dataset."
             ),
             
             sliderInput("date", "Select Date:",
                         min = as.Date("2000-01-01"), 
                         max = as.Date("2025-01-01"), 
                         value = as.Date("2010-06-01"),
                         timeFormat = "%Y-%m-%d"),
             leafletOutput("drought_biplot"),
             
             #### not sure here if the checkbox will be best option here...
             checkboxGroupInput("checkbox_menu", 
                                label = "Select Symbology",
                                choices = list("County" = "opt1", 
                                               "Drought Index" = "opt2"), 
                                selected = NULL),  # Default selection (None selected)
             plotOutput("pca_biplot")
    ),
    
    ############ CLIMATE FACTORS - SL ############
    tabPanel("Climate Trends", 
             h3("Understanding Climate Trends in Two California Counties"),
             p("Understanding drought requires an examination of a region’s overall climate. 
               Yearly precipitation (rain and snow), temperature, and soil moisture all contribute to drought risk. 
               To explore these trends for Los Angeles and El Dorado counties, select a county and a climate variable from the dropdown menus below."),
             
             selectInput("county_cl",
                         label = "Select County",
                         choices = NULL),
             selectInput("climate_factor",
                         label = "Select Climate Variable",
                         choices = NULL),
             plotOutput("climate_plot")
    ),
    
    
    ############ EJ - RB + TB ############
    tabPanel("Environmental Justice",
             h3("Health and Human Impacts Resulting from Drought"),
             p("Water security, quality, wildfires, air quality, and other issues all can be caused or exastebated by drought.
               Marginalized groups and those with the least resources offten bear the brunt of impacts from drought."),
             
             selectInput("county_ej",
                         label = "Select County",
                         choices = c("Los Angeles", "El Dorado")),
             selectInput("envjustice",
                         label = "Select Impact",
                         choices = c("Water Supply", "Water Quality", "Air Quality (PM)", "Fire Risk", "Poverty")),
             plotOutput("ej_plot")
    )
  )
)


# Define server logic
SERVER <- function(input, output, session) {
  
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
      addTiles() %>%
      setView(lng = -119.5, lat = 37, zoom = 6)  # Centered on California
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
  
  # Load the data reactively
  climate_data <- reactive({
    read_csv(here("data", "monthly_prism_climate.csv"))
  })
  
  # Dynamically update the county choices
  observe({
    data <- climate_data()
    counties <- unique(data$county)
    updateSelectInput(session, "county_cl", choices = counties)
  })
  
  # Dynamically update the climate factor choices from the 'climate factor' column
  observe({
    data <- climate_data()
    factors <- unique(data$`climate_factor`)
    updateSelectInput(session, "climate_factor", choices = factors)
  })
  
  # Filter data based on selected county and climate factor
  filtered_data <- reactive({
    req(input$county_cl, input$climate_factor)  # wait for both inputs
    data <- climate_data() %>%
      filter(county == input$county_cl, `climate_factor` == input$climate_factor)
    data
  })
  
  # Create the plot with year on the x-axis and the value on the y-axis
  output$climate_plot <- renderPlot({
    req(input$climate_factor)
    data <- filtered_data()
    
    ggplot(data, aes(x = date, y = value)) + 
      geom_line(size = 1) +
      theme_minimal() +
      labs(x = "Year", 
           y = "Value", 
           title = paste(input$climate_factor, "Trend in", input$county_cl)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui = UI, server = SERVER)