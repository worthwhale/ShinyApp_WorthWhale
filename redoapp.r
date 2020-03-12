library(tidyverse)
library(shiny)
library(shinythemes)
library(here)
library(leaflet)
library(ggmap)
library(lubridate)
library(plyr)
library(RColorBrewer)
library(wesanderson)
library(ggimage)
library(tidymv)
library(png)
library(IMAGE) 

#Read in vessel and whale data
vessels <- read_csv("shiny_vessels.csv")
sightings <- read_csv("sightings.csv")

#separate year into it's own column
whales_year <- sightings %>%
  select(-X1) %>%
  separate(datetime, into = c("date", "time"), sep = " ") %>%
  separate(date, into = c("month", "day", "year"), sep = "/")

#make table of only total frequency and year
whalesighting_table <- whales_year %>%
  select(year, total) %>%
  na.omit() %>%
  group_by(year) %>%
  dplyr::summarise(total = sum(total)) %>% 
  filter(year == "2012" | year == "2013" | year =="2014" | year == "2015" | year == "2016" | year == "2017" | year == "2018") %>% 
  mutate(category = "whales") %>% 
  rename(c("total" = "count"))

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

# bring in vessel totals
vessel_totals <- read_csv("vessel_totals.csv")

# combine

vessel_whale <- rbind(vesselcount_table, whalesighting_table, vessel_totals) 

##Wrangling for whales and vessel graph 

year_vessel <- vessels %>%
  mutate(year = year(timestamp))

whales_vessels <- bind_rows(sightings, year_vessel)

whales_vessels_sf <- st_as_sf(whales_vessels, coords = c("lon", "lat"), crs = 4326)

whales_sf <- st_as_sf(sightings, coords = c("lon", "lat"), crs = 4326)






# Create my user interface

ui <- navbarPage("Navigation",
                 tabPanel("Summary", tags$img(src = "sw.png", align = "center", height = 500, width=800) ,
                          p("This app allows users to explore sperm whale sightings from 2012 - 2018 , (data missing for 2013) and vessel traffic around the island and traveling in and out of the ports of Portsmouth to the North and Roseau to the South. Vessel traffic is based on individual vessel identification number (MMSI), with data missing from 2016 as well as only December data for 2012. For visualization and analytical reasons, each vessel that visits the area is only accounted for once a year.") ,
                          verbatimTextOutput("summary")
                 ),
                 tabPanel("Interactive Map",
                          verbatimTextOutput("Interactive Map"),
                          "Meet the Whales",
                          leafletOutput(outputId = "whale_map", width="100%",height="800px"),
                          checkboxGroupInput(inputId = "year",
                                             choices = c(unique(whales_sf$year)), 
                                             label = "Select Whale CLuster Sighting Year", 
                                             selected = 2005)
                          
                 ),
                 tabPanel("Vessel Speed Map",
                          verbatimTextOutput("Vessel Speed Map"),
                          
                          sidebarLayout(
                            sidebarPanel("My widgets are here",
                                         radioButtons(inputId = "show_hide",
                                                      label = "Whale Presence Points:",
                                                      choices = c("Whale Presence")),
                                         sliderInput("slider2", 
                                                     label = h3("Vessel Speed Range"), 
                                                     min = 10,
                                                     max = 40,
                                                     value = c(0,0))
                            ),
                            mainPanel("My outputs are here!",
                                      leafletOutput("speed_map"))),
                            
                            
                 tabPanel("Vessel and Whale Abundance Graph",
                          h1("Vessel and Sperm Whale Abundance off West Coast of Dominica 2012-2018"),
                          p("Sperm Whale Sightings and Vessel Categories"),
                          
                          sidebarLayout(
                            sidebarPanel(
                              checkboxGroupInput(inputId = "category",
                                                 label = "Choose an Input",
                                                 choices = c("Sperm Whales" = "whales", "Vessel Totals" = "Total Vessel", "Cruise Ship" = "cruiseship","Merchant"= "merchant","High Speed Ferry"="high_speed_ferry")))
                            
                            
                            ,
                            mainPanel(
                              plotOutput(outputId = "vessel_cat_plot")
                            )
                          )
                 ),
                 
                 theme = shinytheme("flatly")))

##########################################################################################

server <- function(input, output) {
  
  vessel_category <- reactive({
    vessel_whale %>% 
      filter(category %in% input$category)
  })
  
  output$vessel_cat_plot <- renderPlot({
    ggplot(data = vessel_category(),
           aes( x = year, y = count, group = category, color = category)) +
      geom_line() +
      scale_fill_brewer(palette="Set1") + 
      labs(x= "Year", y= "Quantity") +
      theme_minimal() 
    
    
    
  })
  
  speed_select <- eventReactive(input$slider2, {
    vessel_rbind %>% 
      filter(speed>input$slider2[1], speed<input$slider2[2]) 
    
  })
  
  icons <- awesomeIcons(
    icon = 'ship',
    iconColor = 'green',
    markerColor = "black",
    library = 'fa'
  )
  
  output$speed_map <- renderLeaflet({
    leaflet() %>%
      setView(lng = -61.475, lat = 15.4159, zoom = 8) %>% 
      addAwesomeMarkers(data = speed_select(), icon = icons) %>% 
      addCircleMarkers(data = sightings, color = "red") %>% 
      addProviderTiles(providers$Esri.WorldStreetMap,
                       options = providerTileOptions(noWrap = TRUE)
      ) 
    
    
  })
  
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
  
}


shinyApp(ui = ui, server = server) 
