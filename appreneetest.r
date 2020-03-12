library(tidyverse)
library(shiny)
library(shinythemes)
library(here)
library(leaflet)
library(ggmap)
library(lubridate)
library(plyr)
library(sf)
library(fs)
library(RColorBrewer)
library(leaflet.extras)


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

##Wrangling for whales and vessel graph 

year_vessel <- vessels %>%
  mutate(year = year(timestamp))

whales_vessels <- bind_rows(sightings, year_vessel)

whales_vessels_sf <- st_as_sf(whales_vessels, coords = c("lon", "lat"), crs = 4326)

whales_sf <- st_as_sf(sightings, coords = c("lon", "lat"), crs = 4326)



# Create my user interface

ui <- navbarPage("Navigation Bar",
                 tabPanel("Summary",
                          h1("Sperm Whales and Vessel Traffic off the West Coast of Dominica"),
                          p("This application allows users to explore sperm whale distrubution and vessel traffic in and out of the ports of Dominica from 2012-2018")) ,
                 
                 
                 tabPanel("Meet the Whales",
                          leafletOutput(outputId = "whale_map", width="100%",height="800px"),
                          checkboxGroupInput(inputId = "year",
                                      choices = c(unique(whales_sf$year)), 
                                      label = "Select Whale CLuster Sighting Year", 
                                      selected = 2005)
            
                 ),
                 
                 
                 tabPanel("Whale and Vessel Abundance Line Graph",
                          p("Sperm Whale Sightings from 2012-2018"),
                          plotOutput(outputId = "whale_plot"),
                 
                 sidebarLayout(
                   sidebarPanel(
                     radioButtons(inputId = "category",
                                  label = "Choose a Vessel Category:",
                                  choices = c("Cruise Ship"="cruiseship","Merchant"= "merchant","High Speed Ferry"="high_speed_ferry","Passenger"="passenger"))),
                   mainPanel("Whale and Vessel Graph"))) ,
                 
                 tabPanel("Vessel Speeds"),
                 
                 theme = shinytheme("flatly"))






server <- function(input, output, session) {
  
  whale_data <- reactive({
    whales_sf %>%
      filter(year == input$year)
  })
  
  #define color pallate 
  colorpal <- colorFactor(palette = c("#16a085", "#27ae60", "#2980b9", "#8e44ad", "#6D214F", "#e74c3c", "#c0392b", "#d35400", "#f39c12", "#1e272e", "#2c3e50", "#7f8c8d"), levels = c("2005", "2007", "2008", "2009", "2010", "2011", "2012", "2014", "2015", "2016", "2017", "2018"))

  
  
  #create leaflet map  
  output$whale_map <- renderLeaflet({
    leaflet() %>%
      setView(lng = -61.475, lat = 15.4159, zoom =10) %>%
      addCircles(data = whale_data(), weight = 10, color = ~colorpal(year), fillOpacity = 1) %>%
      addLegend(position = "bottomright", pal = colorpal, values = whales_sf$year,
                title = "Year of Whale Sighting",
                opacity = 1) %>%
      addProviderTiles(providers$Esri.WorldStreetMap,
                       options = providerTileOptions(noWrap = TRUE)
      ) 
  })
  
  #observe({
    #leafletProxy("whale_map") %>%
    #addCircles(data = whale_data(), weight = 1, fillOpacity = 0.7)
  #})
  
  output$whale_plot <- renderPlot({
    
    ggplot(data = whalesighting_table, aes(x=Var1, y=Freq)) +
      geom_point()
    
  })
  

  
  
  
}


shinyApp(ui = ui, server = server)
