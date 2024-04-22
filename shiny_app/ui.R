library(shiny)
library(bslib)
library(leaflet)

# UI definition (remains unchanged)
ui <- fluidPage(
  titlePanel("Interactive Leaflet Plot"),
  sidebarLayout(
    sidebarPanel(
      
      selectInput("year", "Select Year:", choices = unique(shootings$year)),
      selectInput("race", "Select Race:", choices = c("All", unique(shootings$race))),
      sliderInput("top_states", "Number of Top States to Display:", min = 5, max = 25, value = 5),
      
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
      leafletOutput("map"),
      plotOutput("barplot")
    )
  )
)