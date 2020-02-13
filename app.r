library(tidyverse)
library(shiny)
library(shinythemes)
library(here)
library(leaflet)
library(ggmap)
library(lubridate)

#Read in cluster_data.csv

clusters <-read_csv("clusters.csv")
vessels <- read_csv("shiny_vessels.csv")

#Get Whale Data to Be in Year format instead of datetime
clusters <- clusters %>% 
  mutate(parsedate = mdy_hm(datetime)) %>% 
  mutate(year = lubridate::year(parsedate)) 


# Create my user interface

ui <- navbarPage("Navigation Bar",
                 tabPanel("Summary",
                          h1("Sperm Whales and Vessel Traffic off the West Coast of Dominica"),
                 p("This application allows users to explore sperm whale distrubution and vessel traffic in and out of the ports of Dominica from 2012-2018")),
                 
                 
                 tabPanel("Sperm Whale and Vessel Interaction Map"),

                          
                 tabPanel("Whale Abundance Line Graph",
                          p("Sperm Whale Sightings from 2012-2018"),
                         plotOutput(outputId = "whale_plot")),
                 
                 tabPanel("Vessel Traffic Abundance Line Graph"),
                 
                 theme = shinytheme("flatly"))
        

  
  
  
  
server <- function(input, output) {
  
  output$whale_plot <- renderPlot({
    
    ggplot(data = clusters, aes(x=datetime, y=cluster)) +
      geom_point()
  })
  

  
}


shinyApp(ui = ui, server = server)
