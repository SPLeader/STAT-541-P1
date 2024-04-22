library(shiny)
library(maps)
library(mapproj)
library(bslib)
library(leaflet)
source("helpers.R")
counties <- readRDS(here::here("data/counties.rds"))

# UI definition (remains unchanged)
ui <- fluidPage(
  titlePanel("Interactive Leaflet Plot"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "dataset",
        "Choose Dataset:",
        choices = c("Cities", "Volcanoes")
      )
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)