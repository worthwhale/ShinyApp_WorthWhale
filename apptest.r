library(tidyverse)
library(shiny)
library(shinythemes)
library(here)
library(leaflet)
library(ggmap)
library(lubridate)
library(plyr)

#Read in cluster_data.csv

clusters <-read_csv("clusters.csv")
vessels <- read_csv("shiny_vessels.csv")
sightings <- read_csv("sightings.csv")

#Get Whale Data to Be in Year format instead of datetime
clusters <- clusters %>% 
  mutate(parsedate = mdy_hm(datetime)) %>% 
  mutate(year = lubridate::year(parsedate)) 

cluster_table <- table(clusters$year)
whale_table <- as.data.frame(cluster_table)

sightings <- sightings %>% 
  mutate(parsedate = mdy_hm(datetime)) %>% 
  mutate(year = lubridate::year(parsedate)) 

sighting_table <- table(sightings$year)
whalesighting_table <- as.data.frame(sighting_table)

# getting totals of vessels for each year and category going over 10 knots

year_category <- vessels %>% 
mutate(year = year(timestamp))

v_2012 <- year_category %>% 
  filter(year == "2012") %>% 
  distinct(mmsi, .keep_all= TRUE)

v_2013 <- year_category %>% 
  filter(year == "2013") %>% 
  distinct(mmsi, .keep_all= TRUE)

v_2014 <- year_category %>% 
  filter(year == "2014") %>% 
  distinct(mmsi, .keep_all= TRUE)

v_2015 <- year_category %>% 
  filter(year == "2015") %>% 
  distinct(mmsi, .keep_all= TRUE)

v_2017 <- year_category %>% 
  filter(year == "2017") %>% 
  distinct(mmsi, .keep_all= TRUE)

v_2018 <- year_category %>% 
  filter(year == "2018") %>% 
  distinct(mmsi, .keep_all= TRUE)

vessel_rbind <- rbind(v_2012, v_2013, v_2014, v_2015, v_2017, v_2018 )

# counts of each year of each category
vessel_graph <- table(vessel_rbind$category, vessel_rbind$year)

# make into dataframe for graph

vesselcount_table <- as.data.frame(vessel_graph) %>% 
  rename(c("Var1" = "category")) %>% 
  rename(c("Var2" = "year")) %>% 
  rename(c("Freq" = "count"))



 

# Create my user interface

ui <- navbarPage("Navigation Bar",
                 tabPanel("Summary",
                          h1("Sperm Whales and Vessel Traffic off the West Coast of Dominica"),
                          p("This application allows users to explore sperm whale distrubution and vessel traffic in and out of the ports of Dominica from 2012-2018")) ,

                 
                 tabPanel("Sperm Whale and Vessel Interaction Map"),
                 
                 
                 tabPanel("Whale and Vessel Abundance Line Graph",
                          p("Sperm Whale Sightings from 2012-2018"),
                          plotOutput(outputId = "whale_plot")),
              
                 sidebarLayout(
                 sidebarPanel(
                                radioButtons(inputId = "category",
                                             label = "Choose a Vessel Category:",
                                             choices = c("Cruise Ship"="cruiseship","Merchant"= "merchant","High Speed Ferry"="high_speed_ferry","Passenger"="passenger"))),
                   mainPanel("Whale and Vessel Graph")) ,
                 
                 tabPanel("Vessel Speeds"),
                 
                 theme = shinytheme("flatly"))






server <- function(input, output, session) {
  
  output$whale_plot <- renderPlot({
    
    ggplot(data = whalesighting_table, aes(x=Var1, y=Freq)) +
      geom_point()
  })
  
  
  
}


shinyApp(ui = ui, server = server)
