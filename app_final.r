library(tidyverse)
library(shiny)
library(shinythemes)
library(here)
library(leaflet)
library(ggmap)
library(lubridate)
library(plyr)
library(RColorBrewer)
library(ggimage)
library(tidymv)
library(png)
library(IMAGE) 
library(sf)


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
whales_sf <- st_as_sf(whales_year, coords = c("lon", "lat"), crs = 4326)


####################################################################################################################

# Create my user interface

ui <- navbarPage("Navigation Bar",
                 theme = shinytheme("flatly") ,
                 tabPanel("Summary", tags$img(src = "sw.png", align = "center", height = 500, width=800) ,
                          p("This app allows users to explore sperm whale sightings and vessel traffic from 2012 – 2018 off the west coast of Dominica. Due to extreme weather events, data is missing for 2013 for whale sightings, and 2016 and most of 2012 for vessels. Whale sighting points are documented as clusters, meaning the research team sighted a group of sperm whales on the surface with coordinated behavior and within close proximity of each other. A total of 521 individual sperm whales have been identified in the eastern Caribbean via photo-identification, primarily off of Guadeloupe and Dominica. Vessel traffic is based on individual vessel identification number (Maritime Mobile Service Identity - MMSI). For data processing reasons, each vessel that visits the area is only accounted for once a year in this app. This app should be used as a visualization tool and not for analytical purposes. While navigating the app, users can explore the abundance of vessels and whales from 2012-2018, and visualize areas where vessels may be a threat to sperm whales due to their speed.") ,
                          p("Data Source: Sperm Whale Sightings and AIS Vessel Data - Dr.Shane Gero, Dominica Sperm Whale Project. Photo Credit: Amanda Cotton"),

                          verbatimTextOutput("summary")
                 ),
                 tabPanel("Meet the Whales",
                          p("Sperm whales (Physeter macrocephaslus) are the largest toothed whales who are commonly found in depths of 1000m or deeper. The resident community of sperm whales found in the eastern caribbean are behaviorally distinct due to their vocalizations(clicks). Use the year inputs below to visualize sperm whale distrubution from 2012-2018.") ,
                          leafletOutput(outputId = "whale_map", width="100%",height="800px"),
                          sidebarPanel(
                                       checkboxGroupInput(inputId = "year",
                                                          choices = c(unique(whales_sf$year)), 
                                                          label = "Select Whale Cluster Sighting Year",
                                                          selected = 2005))
                 ),
                 tabPanel("Whale and Vessel Abundance and Interactions",
                          p("Research shows that vessels travelling 10 knots or higher pose a greater threat to whales, and when ships reduce their speed to 10 knots or less, the mortality risk is reduced by 50%. To display vessel abundance at intervals of interest, use the slider bar to select range.") ,
                          p("The line graph plots number of vessels and whales sighted per year from 2012- 2018 off the west coast of Dominica. Number of  vessels is measured by each individual MMSI that came to Dominica each year, if any vessel returned, it is not included in the number of vessels. The whale abundance is measured in number of whales within each cluster sighting per year. A whale can be seen multiple days a  year in different clusters."),
                          verbatimTextOutput("Vessel Speed Map"),
                          sidebarLayout(
                            sidebarPanel(
                                         radioButtons(inputId = "show_hide",
                                                      label = "Whale Presence Points:",
                                                      choices = c("Whale Presence")),
                                         sliderInput("slider2", 
                                                     label = h3("Vessel Speed Range"), 
                                                     min = 10,
                                                     max = 40,
                                                     value = c(0,0))
                            ),
                            mainPanel(
                                      leafletOutput("speed_map")
                                      )
                ),
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
                 ) ,
                
                theme = shinytheme("flatly")))
  

###############################################################################################################

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
    iconColor = 'blue',
    markerColor = "white",
    library = 'fa'
  )
  
  output$speed_map <- renderLeaflet({
    leaflet() %>%
      setView(lng = -61.475, lat = 15.4159, zoom = 8) %>% 
      addAwesomeMarkers(data = speed_select(),icon = icons) %>% 
      addCircles(data = sightings, weight = 7 ,color = "red") %>% 
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
