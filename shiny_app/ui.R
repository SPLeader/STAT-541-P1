library(shiny)
library(bslib)
library(leaflet)

# UI definition (remains unchanged)
ui <- fluidPage(
  titlePanel("Interactive Leaflet Plot"),
  sidebarLayout(
    sidebarPanel(
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
      leafletOutput("map")
    )
  )
)