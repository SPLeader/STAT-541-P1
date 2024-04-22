library(shiny)
library(leaflet)
library(tidyverse)
library(here)

shootings <- read_csv(here("data", "shootings_clean.csv")) %>% 
  drop_na(longitude, latitude, name) %>% 
  group_by(state) %>% 
  sample_frac(0.1) %>% 
  ungroup() 

is_true <- function(x) {
  return(x == TRUE)
}

# Server logic
server <- function(input, output) {
  
  
  map_df = reactive({

    shootings %>%
      select(latitude, longitude, name, input$weapon) %>% 
      filter(rowSums(across(all_of(input$weapon))) >= 1)
  })
  
  # Render leaflet map based on selection
  output$map <- renderLeaflet({
    
    map_df() %>% 
    leaflet() %>%
      addTiles() %>% # Add base tiles
      addCircleMarkers(
                 clusterOptions = markerClusterOptions(), 
                 lng = ~longitude, 
                 lat = ~latitude, 
                 popup = ~name)
  })
}