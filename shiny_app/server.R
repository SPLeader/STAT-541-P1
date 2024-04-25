library(shiny)
library(leaflet)
library(tidyverse)
library(here)
library(sf)
library(dplyr)
library(ggplot2)




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
  
  # Take 5% of the shootings from each state (stratified sample)
  group_by(state) %>% 
  sample_frac(0.05) %>% 
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
        title = str_c("Observed Number of Fatal Shootings by Race (", 
                      input$year,
                      ")"),
        subtitle = str_c("State: ", input$state),
        fill = ""
      ) +
      scale_fill_brewer(palette = "Set2") + 
      theme_minimal()
  },
  res = 96)
  
 
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
          x = as.numeric(year), 
          y = count,  
          color = body_camera)) +
      geom_line(size = 1) +
      scale_x_continuous(breaks = seq(min(plot_data$year), 
                                      max(plot_data$year), by = 1)) +
      geom_vline(xintercept = as.numeric(input$year)) +
      labs(
        x = "Year", 
        y = "",
        color = "Body Camera Presence",
        title = str_c("Number of Fatal Police Shootings in ", input$state),
        subtitle = str_c("Filtered to only ", input$race, " victims")
        ) +
      theme_minimal() +
      scale_color_manual(values = c("red", "blue")) 
  },
  res = 96)
  
  
  
  ## Leaflet plot
  
  # Create a reactive dataframe from the sampled shootings
  map_df = reactive({

    shootings %>%
      
      # Create indicator saying how many of the weapons the shooting involved
      mutate(valid_shooting = rowSums(across(all_of(input$weapon)))) %>% 
      
      # Only include shootings that involved the selected weapon(s)
      filter(valid_shooting >= 1,
             
             year == as.numeric(input$year)) 
      
  })
  
  # Create a reactive dataframe for the states themselves
  state_df = reactive({
    
    # Start with the shootings + state geometry data
    full_data %>%
      
      # Again, filter to only those involving the weapons of interest
      mutate(valid_shooting = rowSums(across(all_of(input$weapon)))) %>% 
      filter(valid_shooting >= 1) %>% 
      
      # Within each state
      group_by(state) %>% 
      
      # Calculate the number of shootings per person in the state
      # (Using 2015 population estimates)
      mutate(shoot_per_cap = n() / total_pop_15) %>% 
      
      # Remove grouping
      ungroup() %>% 
      
      # Extract the distinct values for each state, 
      # shootings per capita, and the geometry
      distinct(NAME, shoot_per_cap, geometry)
  })
  
  # Create leaflet plot
  output$map <- renderLeaflet({
    
    # Create leaflet
    leaflet() %>%
      
      # Add base tiles
      addTiles() %>%
      
      # Add markers to the plot
      addMarkers(
        
        # Make them cluster when you zoom out for clarity
        clusterOptions = markerClusterOptions(), 
        
        # Plot the latitudes from the interactive dataframe
        lng = map_df()$longitude, 
        
        # Plot the longitudes from the interactive dataframe
        lat = map_df()$latitude, 
        
        # Specify the popup information when clicking on shootings
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
      
      # Add polygons for each state
      addPolygons(data = state_df()$geometry,
                  
                  # Specify outline and fill options 
                  color = "#444444", 
                  weight = 1, 
                  smoothFactor = 0.5,
                  opacity = 0.5, 
                  fillOpacity = 0.5,
                  
                  # Popup message when clicking on each state
                  popup = str_c("State: ",
                                state_df()$NAME,
                                "<br>",
                                "Fatal shootings per 100,000 residents from 2015-2023: ",
                                round(state_df()$shoot_per_cap * 100000, 2)),
                  
                  # Color by shootings per capita involving selected weapon(s)
                  fillColor = colorQuantile(
                    "YlOrRd", 
                    state_df()$shoot_per_cap)(state_df()$shoot_per_cap),
                  
                  # Add a white highlight on the border when you hover
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 2,
                                                      bringToFront = TRUE)) 
  })
  
  #create function that takes in user input for widget summary table
  
  widget_tbl = reactive({
    selected <- input$variable
  

    #if numerical variables age is selected, perform numerical five number summary
    
    if(selected %in% c("age"))
      
    {
      summary <- shootings %>%
        summarize(`Mean Age` = mean(age, na.rm = TRUE),
                  `SD Age` = sd(age, na.rm = TRUE), 
                  `IQR Age` = IQR(age, na.rm = TRUE), 
                  `Min. Age` = min(age, na.rm = TRUE),
                  `Max. Age`= max(age, na.rm = TRUE))
    }


    #if categorical variables are selected perform categorical summary
    # return count and percent of total 

    else 
      
    {
      summary <- shootings %>%
        group_by(!!sym(selected)) %>%
        summarise(n = n()) %>%
        mutate(freq = round(n/sum(n) * 100, 2))
      
      summary <- as.data.frame(summary)
      
      selected = selected %>% 
        str_replace_all("_", " ") %>% 
        str_to_title()
      
      colnames(summary) <- c(selected, "Count", "Percent of Total")
      
      
    }

    return(summary)

  })
  
  #widget table output for summary statistics
  #formatting: adding stripes, hover, rounding digits to 2
  
  output$table <- renderTable({
  
    widget_tbl() }, 
    striped = TRUE, 
    hover = TRUE, 
    bordered = TRUE, 
    digits = 2
  

)
  
  
}  