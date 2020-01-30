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
              tableOutput(outputId = "sighting_table"),
              
              leafletOutput("mymap"),
              p(),
              actionButton("recalc", "New points")
    )
    )
  )

server <- function(input, output,session) {
  
  date_select <- reactive({
    clusters %>%
      select(datetime, lat, lon)
    
  })
  
  output$sighting_table <- renderTable({
  date_select()
    
  })
  
}


shinyApp(ui = ui, server = server)
