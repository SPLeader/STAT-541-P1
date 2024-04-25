library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(tidyverse)
library(here)

# Read in individual shootings dataframe
shootings <- read_csv(here("data", "shootings_clean.csv")) %>% 
  
  # Drop any missing values for longitude, latitude, or name
  drop_na(longitude, latitude, name) %>% 
  
  # Add a column containing the year the shooting took place
  mutate(year = year(ymd(date)))


# UI definition (remains unchanged)
ui <- page_fillable(
  
  title = "Fatal Police Shootings (since 1/1/2015)",
  
  h6("Data source: ", 
     a(href = "https://www.washingtonpost.com/graphics/investigations/police-shootings-database/", 
       "The Washington Post")),
  
  card(
    card_header("Race and Weapon"),
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          "year", 
          "Select Year:", 
          choices = seq(
            min(shootings$year), 
            max(shootings$year)
          )
        ),
        
        selectInput(
          "state", 
          "Select State:",
          choices = c(
            "All",
            sort(
              unique(shootings$state)
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
    
    
  ),
  
  card(
    card_header("Leaflet Plot"),
    layout_sidebar(
      sidebar = sidebar(
        checkboxGroupInput(
          "weapon",
          "Select all that apply",
          choices = list("Gun" = "gun", 
                         "Knife" = "knife", 
                         "Replica" = "replica", 
                         "Unarmed" = "unarmed"),
          selected = 1
        )
      ),
      leafletOutput("map")
    )
    
  ), 
  
  
  tabPanel("Summary Table",
  card(
    card_header("Summary Table"), 
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          "variable", 
          "Variable Selection", 
          choices = list(
                   "Threat Type" = "threat_type",
                   "Flee Status" = "flee_status",
                   "Armed With" = "armed_with",
                   "City" = "city",
                   "State" = "state",
                   "County" = "county",
                   "Age" = "age",
                   "Gender" = "gender",
                   "Race" = "race",
                   "Mental Illness Related" = "was_mental_illness_related",
                   "Body Camera" = "body_camera",
                   "Gun" = "gun",
                   "Replica" = "replica",
                   "Knife" = "knife",
                   "Unarmed" = "unarmed"), 
            selected = 1
         )
         
        ),
    
      tableOutput("table") 

    ) 
  )
))
  

  

  