library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(tidyverse)
library(here)
library(sf)

# Create a dataframe with state information from prebuilt R vectors
state_info <- data.frame(
  NAME = state.name,
  state = state.abb
)

# Read in individual shootings dataframe
shootings <- read_csv(here("data", "shootings_clean.csv")) %>% 
  
  # Drop any missing values for longitude, latitude, or name
  drop_na(longitude, latitude, name) %>% 
  
  # Add a column containing the year the shooting took place
  mutate(year = year(ymd(date))) %>% 
  
  inner_join(state_info, by = "state") %>% 
  
  inner_join(spData::us_states, by = "NAME")


# UI definition (remains unchanged)
ui <- page_fillable(
  
  theme = bs_theme(preset = "lux"),
  
  #add theme and reference data source
  card(
    min_height = 250,
    card_header("Fatal Police Shootings (since 1/1/2015)"),
    h6("Data source: ", 
       a(href = "https://www.washingtonpost.com/graphics/investigations/police-shootings-database/", 
         "The Washington Post")),
    
    "Authors: Sean Leader, Rebecca Ioffe, and Ashley Ibarra",
    br(),
    br(),
    "This interactive application allows you to explore data on shootings in the United States of America from 2015 to 2023. ",
    br(),
    br(),
    "Created for STAT 541 - Advanced Statistical Computing with R at Cal Poly SLO, 2024."
    
  ),
  
  fluidRow(
    column(
      width = 6, 
   card(
     min_height = 1200,
    card_header("Line and Bar plots showing aggregated statistics"),
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          "year", 
          "Select Year:", 
          choices = seq(
            min(shootings$year), 
            max(shootings$year) - 1
          )
        ),
        
        selectInput(
          "NAME", 
          "Select State:",
          choices = c(
            "All",
            sort(
              unique(shootings$NAME)
            )
          )
        ),
        
        selectInput(
          "race", 
          "Select Race:",
          choices = c(
            "All", 
            unique(shootings$race)
          )
        )
      ),
      plotOutput("bodyCamPlot"),
      plotOutput("racePlot")
    )
   )
   
  ),
  
  column(
    width = 6, 
    card(
      min_height = 600,
    card_header("Map of Fatal Shootings across the United States"),
    layout_sidebar(
      sidebar = sidebar(
        checkboxGroupInput(
          "weapon",
          "Select weapons of interest",
          choices = list("Gun" = "gun", 
                         "Knife" = "knife", 
                         "Replica" = "replica", 
                         "Unarmed" = "unarmed"),
          selected = 1
        ),
        helpText("Note: Map sensitive to input variables other than state."),
        
       
      ),
      leafletOutput("map")
    )
   
 ), 
  
   card(
     min_height = 600,
    card_header("Summary Table for Selected Variable"), 

    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          "variable", 
          "Variable Selection", 
          choices = list(
                   "Flee Status" = "flee_status",
                   "Age" = "age",
                   "Gender" = "gender",
                   "Race" = "race",
                   "Mental Illness Related" = "was_mental_illness_related",
                   "Body Camera" = "body_camera",
                   "Gun" = "gun",
                   "Replica" = "replica",
                   "Knife" = "knife",
                   "Unarmed" = "unarmed", 
                   "Threat Description" = "threat_description"), 
            selected = 1 ),
            helpText("Note: Table sensitive to input variables other than weapon.")
         
         
        ),
    
      tableOutput("table") 

      ) 
     )
    )
  )
)
  

  

  