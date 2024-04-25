library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(tidyverse)
library(here)

shootings <- read_csv(here("data", "shootings_clean.csv")) %>% 
  drop_na(longitude, latitude, name) 

# UI definition (remains unchanged)
ui <- fluidPage(
  titlePanel(title="Fatal Police Shootings (since 1/1/2015)", windowTitle="Fatal Police Shootings"),
  h6("Data source: ", a(href = "https://www.washingtonpost.com/graphics/investigations/police-shootings-database/", "The Washington Post")),
  
  sidebarLayout(
    sidebarPanel(
      

      sliderInput("year", "Select Year:", min = min(as.numeric(shootings$year)), 
                  max = max(as.numeric(shootings$year)), value = min(shootings$year), step = 1),
      selectInput("state", "Select State:", choices = c("All", unique(shootings$state))),
      
      selectInput("race", "Select Race:", choices = c("All", unique(shootings$race))),
      
      
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
    mainPanel(
     plotOutput("racePlot"),
     plotOutput("bodyCamPlot"),
     leafletOutput("map")
    )
  )
)