library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(here)

# Define the data directory
data_dir <- here("data", "drought_index")

# Get all `.shp` files recursively
shp_files <- list.files(data_dir, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
california_shp <- here("data", "ca_state")

# Read California boundary
ca_boundary <- st_read(california_shp) %>% 
  st_transform(4326)

# Extract dates from filenames and sort them
shp_data <- data.frame(
  file_path = shp_files,
  year = as.numeric(substr(gsub(".*USDM_(\\d{8})\\.shp", "\\1", basename(shp_files)), 1, 4))
) %>% 
  arrange(year) %>% 
  distinct(year, .keep_all = TRUE)  # Keep only one file per year

# Define UI
ui <- fluidPage(
  titlePanel("Drought Index Over Time (California)"),
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
)

# Define Server
server <- function(input, output, session) {
  
  initial_year <- min(shp_data$year)  # Get earliest year
  initial_file <- shp_data %>%
    filter(year == initial_year) %>%
    pull(file_path)
  
  # Load the initial shapefile to ensure the map is not blank at start
  initial_shp <- if (length(initial_file) > 0) {
    st_read(initial_file[1]) %>%
      st_transform(4326) %>%
      st_make_valid() %>%
      st_intersection(ca_boundary)  # Clip to California
  } else {
    NULL
  }
  
  # Define the reactive dataset
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
  
  # Render the map with the default (first time step) dataset
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%  
      setView(lng = -119.5, lat = 37, zoom = 6) %>%  # Centered on California
      {
        if (!is.null(initial_shp)) {
          addPolygons(., data = initial_shp, 
                      fillColor = ~colorFactor(
                        palette = c("#FFFF00", "#FBD47F", "#FFAA01", "#E60001", "#710001"), 
                        domain = c(0, 1, 2, 3, 4)
                      )(initial_shp$DM), 
                      color = NA,
                      weight = 0, 
                      fillOpacity = 1,
                      popup = ~paste("Drought Index:", DM))
        } else {
          .
        }
      }
  })
  
  # Observe tab changes and reset map + time slider
  observeEvent(input$tabs, {
    if (input$tabs == "Drought Map") {
      leafletProxy("map") %>%
        setView(lng = -119.5, lat = 37, zoom = 6)  # Reset view to California
      
      updateSliderInput(session, "year", value = min(shp_data$year))  # Reset to the first year
    }
  })
  
  # Observe changes in the year selection and update the map
  observe({
    req(selected_shp())
    
    # Define color palette
    pal <- colorFactor(
      palette = c("#FFFF00", "#FBD47F", "#FFAA01", "#E60001", "#710001"), 
      domain = c(0, 1, 2, 3, 4)
    )
    
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(data = selected_shp(), 
                  fillColor = ~pal(DM),  
                  color = NA,
                  weight = 0, 
                  fillOpacity = 1,
                  popup = ~paste("Drought Index:", DM))
  })
  
#   selected_shp <- reactive({
#     req(input$year)
#     
#     file_to_load <- shp_data %>%
#       filter(year == input$year) %>%
#       pull(file_path)
#     
#     if (length(file_to_load) > 0) {
#       drought_data <- st_read(file_to_load[1]) %>% 
#         st_transform(4326) %>% 
#         st_make_valid()  # Fix invalid geometries
#       
#       drought_ca <- st_intersection(drought_data, ca_boundary)  # Clip to California
#       
#       return(drought_ca)
#     } else {
#       return(NULL)
#     }
#   })
#   
#   output$map <- renderLeaflet({
#     leaflet() %>%
#       addTiles() %>%
#       setView(lng = -119.5, lat = 37, zoom = 6)  # Centered on California
#   })
#   
#   observe({
#     req(selected_shp())
#     
#     # Define updated color palette for DM values (no color for 5)
#     pal <- colorFactor(
#       palette = c("#FFFF00", "#FBD47F", "#FFAA01", "#E60001", "#710001"), 
#       domain = c(0, 1, 2, 3, 4)  # Exclude 5 from domain
#     )
#     
#     leafletProxy("map") %>%
#       clearShapes() %>%
#       addPolygons(data = selected_shp(), 
#                   fillColor = ~pal(DM),  # Assign color based on DM values
#                   color = NA,       # Outline color
#                   weight = 0, 
#                   fillOpacity = 1,
#                   popup = ~paste("Drought Index:", DM))  # Add popup info
#   })
# }
# 
# shinyApp(ui, server)
