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
  titlePanel("California Drought Explorer"),
  
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
             h3("How has the distribution of drought conditions changed over time?"),
             fluidRow(
               column(6,
                      p(HTML("The U.S. Drought Monitor (USDM) has mapped drought conditions across the United States since 2000, providing real-time snapshots of drought severity.
                           Spatial drought monitoring is useful for decision-making in areas like water management, agriculture, and emergency response.
                           The USDM integrates multiple indicators, including precipitation, streamflow, reservoir levels, temperature, evaporative demand, soil moisture, and vegetation health.
                           The data in this map represents annual drought conditions during the peak drought season in late August. 
                           <b>Use the time slider below to explore how drought conditions have evolved over time.</b>")),
                      sliderInput("year", "Select Year:",
                                  min = min(shp_data$year), 
                                  max = max(shp_data$year),
                                  value = min(shp_data$year), 
                                  step = 1, 
                                  animate = TRUE, 
                                  sep = "",
                                  width = "100%"),
                      h4("Drought Index Categories"),
                      DTOutput("drought_table")
               ),
               column(6, 
                      leafletOutput("map", height = "100vh")
               )
             )
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
             h3("Understanding County-level Climate Trends"),
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
             h3("Health and Human Impacts Related to Drought"),
             p("Water security, quality, wildfires, air quality, and other issues all can be caused or exastebated by drought.
               Marginalized groups and those with the least resources often bear the brunt of impacts from drought."),
             
             selectInput("ej_variable",
                         label = "Select EJ Factor",
                         choices = NULL),
             plotOutput("ej_box_plot"),
             tableOutput("meta_data")
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
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%  # Change to CartoDB Positron basemap
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
                searching = FALSE  
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

########

# EJ tab

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
    geom_boxplot() +
    theme_minimal() +
    labs(x = "County", 
         y = "Value", 
         title = paste(input$ej_variable)) +
    scale_fill_brewer(palette = "Pastel1") +
    theme(legend.position = "none")
})

# Render the table
output$meta_data <- renderTable({
  meta_data()
})

}

shinyApp(ui = UI, server = SERVER)




shinyApp(ui = UI, server = SERVER)