library(shiny)
library(leaflet)
library(tidyverse)
library(here)
library(sf)
library(dplyr)
library(ggplot2)
library(DT)



# Read in and clean data

# Read in individual shootings dataframe
shootings <- read_csv(here("data", "shootings_clean.csv")) %>% 
  
  # Drop any missing values for longitude, latitude, or name
  drop_na(longitude, latitude, name) %>% 
  
  # Add a column containing the year the shooting took place
  mutate(year = year(ymd(date)))

# Choose subsample of shootings to plot on map 
# (too many individual points creates a rendering issue)
shootings_sample  <- shootings %>% 
  
  # Take 10% of the shootings from each state (stratified sample)
  group_by(state) %>% 
  sample_frac(0.1) %>% 
  ungroup() 

# Create a dataframe with state information from prebuilt R vectors
state_info <- data.frame(
  NAME = state.name,
  state = state.abb
)

# Get spatial data for plotting polygons in addition to individual points
full_data <- spData::us_states %>% 
  
  # Add this to the state info dataframe (just used to link to shootings)
  inner_join(state_info, by = "NAME") %>% 
  
  # Add in shootings data
  inner_join(shootings, by = "state")












# Server logic
server <- function(input, output) {

  
  # Render the race breakdown plot
  output$racePlot <- renderPlot({
    
    # If the person has selected a particular state
    if(input$state != "All") {
      
      # Filter the data by both year and state
      filtered_data <- shootings %>% 
        filter(
          year == input$year, 
          state == input$state
        )
    } 
    
    # If the person has not yet input a state
    else {
      
      # Filter only using the input year
      filtered_data <- shootings %>% 
        filter(year == input$year)
    }
    
    filtered_data %>%
      
      # Add custom descriptions for legend
      mutate(Weapon = case_when(
        gun ~ "Gun Involved",
        !gun ~ "No Gun Involved",
        is.na(gun) ~ "Unknown"
      )) %>% 
      
      # Cross-tabulate race and weapon counts
      count(race, Weapon) %>% 
      
      # Create segmented bar chart by race and weapon
      ggplot(aes(
        x = fct_reorder(race, n), 
        y = n, 
        fill = Weapon
      )) + 
      
      # Specify bar chart
      geom_col() +
      
      # Add axis labels
      labs(
        x = "Race of Victim",
        y = "",
        title = "Observed Number of Fatal Shootings by Race",
        fill = ""
      )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ## Body Cam Plot
  
  # Filter dataset according to input
  filtered_data <- reactive({
    
    data <- shootings
    
    # If the user has inputted a state
    if(input$state != "All") {
      
      # Filter the data to that state
      data <- filter(data, state == input$state)
    }
    
    # If the user has inputted a race
    if(input$race != "All") {
      
      # Filter the data to only that race
      data <- filter(data, race == input$race)
    }
    
    # Return data
    data
  })
  
  # Create the plot
  output$bodyCamPlot <- renderPlot({
    
    # Use the reactive data using specified inputs
    plot_data <- filtered_data() %>%
      
      # Group by year and body camera presence
      group_by(year, body_camera) %>%
      
      # Get count of each unique combo
      summarise(count = n())
    
    # Create plot
    plot_data %>%
      ggplot(
        aes(
          x = year, 
          y = count,  
          color = body_camera)) +
      geom_line(size = 1) +
      labs(
        x = "Year", 
        y = "",
        color = "Body Camera Presence",
        title = "Count of Body Cameras in Fatal Police Shootings Over the Years") +
      theme_minimal()
  })
  
  
  
  # start leaflet plot
  
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
  
  
  # widget_tbl = reactive({
  #   selected <- input$variable
  #   selecteddf <- shootings %>%
  #     select(threat_type,
  #            flee_status,
  #            armed_with,
  #            city,
  #            state,
  #            county,
  #            age,
  #            gender,
  #            race,
  #            was_mental_illness_related,
  #            body_camera,
  #            gun,
  #            replica,
  #            knife,
  #            unarmed, 
  #     )
  #   
  #   if(selected %in% c("threat_type", "flee_status", "armed_with", "city", "state", "county", "gender", "race"))
  #     
  #   {
  #     summary <- selecteddf %>%
  #       group_by(!!sym(selected)) %>%
  #       summarise(n = n()) %>%
  #       mutate(freq = n/sum(n))
  #     
  #   }
  #   
  #   
  #   else {
  #     summary <- selecteddf %>%
  #       summarise(across(
  #         .cols = selected, 
  #         .fns = list(Mean = mean, SD = sd), na.rm = TRUE
  #       ))
  #     
  #   }
  #   
  #   summary <- data.frame(summary)
  #   return(summary)
  #   
  # })
  
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
  
  output$table <- renderDataTable({
    datatable(widget_tbl)} 
    
    # row.name = TRUE, 
    # digits = 2, 
    # striped = TRUE, 
    # bordered = TRUE, 
    # hover = TRUE
    
  )
  
  
  
}