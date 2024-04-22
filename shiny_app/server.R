library(shiny)
library(leaflet)
library(tidyverse)
library(here)

shootings <- read_csv(here("data", "fatal-police-shootings-data.csv")) %>% 
  drop_na(longitude, latitude, name) %>% 
  group_by(state) %>% 
  sample_frac(0.1) %>% 
  ungroup()



# Server logic
server <- function(input, output) {
  

  
  # Render leaflet map based on selection
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>% # Add base tiles
      addCircleMarkers(data = shootings, 
                 clusterOptions = markerClusterOptions(), 
                 lng = ~longitude, 
                 lat = ~latitude, 
                 popup = ~name)
  })
}