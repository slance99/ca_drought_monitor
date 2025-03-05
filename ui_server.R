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

############################################################################
############################################################################
############################################################################

# Define UI define
UI <- fluidPage(
  theme = shinytheme("united"),
  
  # Custom CSS for styling the slider
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
")),

  # Application title
  titlePanel("California Drought Explorer"),
  
  ########### DROUGHT INTRO - SL ###########
  tabsetPanel(
    id = "tabs",
    tabPanel("Background", 
             h3("Understanding Drought Risk in California"),
             tags$img(src = "lake_oroville_drought.jpg", alt = "Sample Image", style = "width: 100%; height: 400;"),
             p("AP Photo/Noah Berger"),
             
             tags$br(), # Add a line break
             
             
             p("With its dry Mediterranean climate, California is particularly vulnerable to drought, which has become more frequent and severe due to the effects of climate change. 
               While droughts are a natural part of the climate system, their intensity and duration are exacerbated by rising temperatures and shifting precipitation patterns, leading to serious impacts on water supply, agriculture, and ecosystems. 
               Recent droughts, such as the five-year drought from 2012 to 2016, have highlighted the urgent need for effective drought management and climate adaptation strategies."),
             
             tags$br(), # Add a line break
             
             p("This Shiny app offers an interactive exploration of drought parameters across California, with a particular focus on the environmental justice implications in Los Angeles and El Dorado County. 
               Through visualizations of climate trends, a principal component analysis of climate variables, and an environmental justice analysis, the app demonstrates the vital role of monitoring drought conditions in managing risk and supporting vulnerable communities. 
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
    
    ############ PCA - RB ############
    tabPanel("Principal Component Analysis", 
             h3("Principle Component Analysis for Environmental Variables Related to Drought"),
             p("Principle Component Analysis (PCA) is an unsupervised machine learning ordination method, 
               or linear dimensionality reduciton. PCA projects a swarm of mutlidimentional data
               onto a two dimentional plot with Principle Components (PC) on each axis chosen based on 
               the direction of the data with the greatest variance. PCA is useful for multidimentional data exploration
               and can tell us a lot about correlations between many variables within a dataset. 
               
               This PCA focuses on climate variables and their relatation to drought conditions within California Counties in 2021."
             ),
             
             plotOutput("biplot"),
             plotOutput("screeplot", height = "350px", width = "75%")
             
    ),
    
    ############ CLIMATE FACTORS - SL ############
    tabPanel("Climate Trends", 
             h3("Understanding Climate Trends for California Counties"),
             fluidRow(
               column(4,
               p(HTML("Climate factors such precipitation, temperature, and vapor pressure deficit play a major role in 
               determining drought conditions. Increased temperature and decreased precipitation can lead to more severe and prolonged droughts.
               Due to climate change, these factors have and will continue to experience significant change over time, impacting drought risk.
               <b> To see how these factors have changed for individual counties in California, select a county and climate factor of interest .</b>")),
               wellPanel(selectInput("county_cl",
                           label = "Select County",
                           choices = NULL),
                         selectInput("climate_factor",
                           label = "Select Climate Variable",
                           choices = NULL)
                         )
               ),
               column(8,
                      plotOutput("climate_plot", height = "350px", width = "100%")
                      ))),
    
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


############################################################################
############################################################################
############################################################################

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
      geom_line(size = 1.2) +  # Slightly thicker line for better visibility
      scale_fill_viridis(option = "plasma") +  # Using fill for color scaling
      theme_classic() +
      labs(x = "Year", 
           y = "Value", 
           title = paste(input$climate_factor, "Trend in", input$county_cl)) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Larger x-axis labels
        axis.text.y = element_text(size = 14),  # Larger y-axis labels
        axis.title.x = element_text(size = 16),  # Larger x-axis title
        axis.title.y = element_text(size = 16),  # Larger y-axis title
        plot.title = element_text(size = 18, hjust = 0.5),  # Larger title
        legend.title = element_text(size = 14),  # Larger legend title
        legend.text = element_text(size = 12)  # Larger legend text
      )
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


########

### PCA tab

# Load the data reactively
joined_drought_data <- reactive({
  read_csv(here("data","joined_drought_data.csv"))
})

  joined_drought_pca <- joined_drought_data |>
    select(where(is.numeric)) |>
    drop_na() |>
    select_if(~ var(.x, na.rm = TRUE) != 0)  # Remove zero-variance columns
  
  # scale
  joined_drought_pca <- joined_drought_pca |> 
    prcomp(scale = TRUE)
  
  # Define custom colors in a vector
  color_values <- c("#FFFF00", "#FBD47F", "#FFAA01", "#E60001", "#710001")
  
  color_palette <- setNames(color_values, c("D0", "D1", "D2", "D3", "D4"))
  
  output$biplot <- renderPlot({
    # Plot the PCA biplot with ggplot2
    autoplot(joined_drought_pca, 
                      data = joined_drought_data, 
                      loadings = TRUE,
                      color = "USDM_index",
                      loadings.label = TRUE,
                      loadings.colour = "black",
                      loadings.label.colour = "black",
                      loadings.label.vjust = -0.5) +
      scale_color_manual(values = color_palette) +
      theme_minimal() +
      labs(title = "PCA of Drought and Climate Conditions in CA Counties (2021)",
             colour = "Drought Index", shape = "County")
})
  
  ### Scree Plots ###
  
  # create a dataframe with the necessary indgreidents to make a screeplot
  
  pc_names <- colnames(joined_drought_pca$rotation)
  sd_vec <- joined_drought_pca$sdev
  var_vec <- sd_vec^2    # sd = variance^2
  
  
  pct_expl_df <- data.frame(v = var_vec,
                            pct_v = var_vec / sum(var_vec),
                            pc = pc_names)
  
  # Screeplot
  output$screeplot <- renderPlot({
    ggplot(pct_expl_df, aes(x = fct_reorder(pc, v, .desc = TRUE), y = v)) +
      geom_col(fill="steelblue") +
      geom_text(aes(label = scales::percent(round(pct_v,3))), vjust = 0, nudge_y = .5, angle=90) +
      labs(title = "Scree Plot of Principle Components", x = 'Principal component', y = 'Variance explained') +
      theme_bw()+
      theme(axis.text = element_text(size=10)) +
      theme(axis.text.x = element_text(angle=45,hjust=1))
})
  
}

shinyApp(ui = UI, server = SERVER)
