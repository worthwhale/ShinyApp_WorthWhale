library(tidyverse)
library(shiny)
library(shinythemes)
library(here)
library(leaflet)
library(ggmap)

#Read in cluster_data.csv

clusters <-read_csv("clusters.csv")

# Create my user interface

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Sperm Whale Distrubution in Dominica"),
  sidebarLayout(
    sidebarPanel("My widgets are here",
                 selectInput(inputId = "date_select",
                             label = "Choose a Year:",
                             choices = unique(clusters$datetime)
                 )
    ),
    mainPanel("My outputs are here!",
              leafletOutput("sighting_map"))
              
    
    )
  )

server <- function(input, output,session) {
  
  date_select <- reactive({
    clusters %>%
      select(lat, lon)
    
  })
  
  output$sighting_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldStreetMap,
                       options = providerTileOptions(noWrap = TRUE)
      ) 
      
 
  })
  
}


shinyApp(ui = ui, server = server)
