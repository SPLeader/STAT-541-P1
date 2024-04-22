library(shiny)
library(leaflet)
library(tidyverse)
library(here)

shootings <- read_csv(here("data", "shootings_clean.csv")) %>% 
  drop_na(longitude, latitude, name) %>% 
  group_by(state) %>% 
  sample_frac(0.1) %>% 
  ungroup() %>% 
  mutate(involved_gun = case_when(involved_gun == "Yes" ~ TRUE,
                         TRUE ~ FALSE),
         involved_knife = case_when(involved_knife == "Yes" ~ TRUE,
                                  TRUE ~ FALSE),
         involved_replica = case_when(involved_replica == "Yes" ~ TRUE,
                                    TRUE ~ FALSE),
         unarmed = case_when(unarmed == "Yes" ~ TRUE,
                             TRUE ~ FALSE))

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