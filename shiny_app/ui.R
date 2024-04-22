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
        choices = list("Gun" = "involved_gun", 
                       "Knife" = "involved_knife", 
                       "Replica" = "involved_replica", 
                       "Unarmed" = "unarmed"),
        selected = 1
      )
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)