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
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Select Year:",
                  min = min(shp_data$year), 
                  max = max(shp_data$year),
                  value = min(shp_data$year), 
                  step = 1, 
                  animate = TRUE, 
                  sep = "")
    ),
    mainPanel(
      leafletOutput("map", height = 600)
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
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
    
    # Define updated color palette for DM values (no color for 5)
    pal <- colorFactor(
      palette = c("#FFFF00", "#FBD47F", "#FFAA01", "#E60001", "#710001"), 
      domain = c(0, 1, 2, 3, 4)  # Exclude 5 from domain
    )
    
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(data = selected_shp(), 
                  fillColor = ~pal(DM),  # Assign color based on DM values
                  color = NA,       # Outline color
                  weight = 0, 
                  fillOpacity = 1,
                  popup = ~paste("Drought Index:", DM))  # Add popup info
  })
}

shinyApp(ui, server)
