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

# combine

vessel_whale <- rbind(vesselcount_table, whalesighting_table)


######################################################################################################

 

# Create my user interface

ui <- navbarPage("Navbar!",
                 tabPanel("Summary" ,
                          p("This app allows users to explore sperm whale sightings from 2012 - 2018 , (data missing for 2013) and vessel traffic in and out of the ports based on individual vessel identification number (MMSI), with data missing from 2016") ,
                          verbatimTextOutput("summary")) ,
                 tabPanel("Interactive Map",
<<<<<<< HEAD
                          verbatimTextOutput("Interactive Map")) ,
=======
                          verbatimTextOutput("Interactive Map")),
                 #Shellby's Tab
>>>>>>> 94e418620a3add1f36164987985ac1002ff5877e
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
                                                          choices = c("Sperm Whales" = "whales", "Cruise Ship" = "cruiseship","Merchant"= "merchant","High Speed Ferry"="high_speed_ferry")))
                   ,
                            mainPanel(
<<<<<<< HEAD
                              plotOutput(outputId = "vessel_cat_plot")))

                
=======
                              plotOutput(outputId = "vessel_cat_plot")
                            )
                          )
                 ),
                 
                 theme = shinytheme("flatly")))
>>>>>>> 94e418620a3add1f36164987985ac1002ff5877e

########################################################################################################                
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
      addProviderTiles(providers$Esri.WorldStreetMap,
                       options = providerTileOptions(noWrap = TRUE)
      ) 
    
    
  })
  
}


shinyApp(ui = ui, server = server) 



