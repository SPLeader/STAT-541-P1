library(shiny)
library(leaflet)
library(tidyverse)
library(here)
library(sf)

shootings <- read_csv(here("data", "shootings_clean.csv")) %>% 
  drop_na(longitude, latitude, name) 

shootings_sample  <- shootings %>% 
  group_by(state) %>% 
  sample_frac(0.1) %>% 
  ungroup() 

state_info <- data.frame(
  NAME = state.name,
  state = state.abb
)

full_data <- spData::us_states %>% 
  inner_join(state_info, by = "NAME") %>% 
  inner_join(shootings, by = "state")

# Server logic
server <- function(input, output) {

  
  output$barplot <- renderPlot({
    filtered_data <- subset(shootings, year == input$year)
    if (input$race != "All") {
      filtered_data <- subset(filtered_data, race == input$race)
    }
    
    # Aggregate data by state
    aggregated_data <- aggregate(id ~ state, data = filtered_data, FUN = sum)
    
    # Calculate the top states based on the number selected by the user
    top_states <- head(aggregated_data[order(aggregated_data$id, decreasing = TRUE), ], input$top_states)
    
    ggplot(top_states, aes(x = state, y = id, fill = state)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = paste("Top", input$top_states, "States with Most Shooting Incidents in", input$year),
           x = "State", y = "Number of Incidents", fill = "State")
  })
  
  
    map_df = reactive({

    shootings_sample %>%
      mutate(valid_shooting = rowSums(across(all_of(input$weapon)))) %>% 
      filter(valid_shooting >= 1) 
      
  })
  
  state_df = reactive({
    
    
    shootings %>%
      mutate(valid_shooting = rowSums(across(all_of(input$weapon)))) %>% 
      filter(valid_shooting >= 1) %>% 
      inner_join(state_info, by = "state") %>% 
      inner_join(spData::us_states, by = "NAME") %>% 
      group_by(state) %>% 
      mutate(shoot_per_cap = n() / total_pop_15) %>% 
      ungroup() %>% 
      distinct(NAME, shoot_per_cap, geometry)
  })
  
  # Render leaflet map based on selection
  output$map <- renderLeaflet({
    
    leaflet() %>%
      addTiles() %>% # Add base tiles
      addMarkers(
        clusterOptions = markerClusterOptions(), 
        lng = map_df()$longitude, 
        lat = map_df()$latitude, 
        popup = str_c("Name: ", 
                      map_df()$name,
                      "<br>",
                      "Date: ",
                      map_df()$date,
                      "<br>",
                      "Weapon: ",
                      map_df()$armed_with,
                      "<br>",
                      "Description: ",
                      map_df()$threat_description)
        ) %>% 
      addPolygons(data = state_df()$geometry,
                  color = "#444444", 
                  weight = 1, 
                  smoothFactor = 0.5,
                  opacity = 0.5, 
                  fillOpacity = 0.5,
                  popup = str_c("State: ",
                                state_df()$NAME,
                                "<br>",
                                "Fatal shootings involving selected weapon(s) per 100,000 residents: ",
                                round(state_df()$shoot_per_cap * 100000, 2)),
                  fillColor = colorQuantile("YlOrRd", 
                                            state_df()$shoot_per_cap)(state_df()$shoot_per_cap),
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 2,
                                                      bringToFront = TRUE))
  })
  
  
}