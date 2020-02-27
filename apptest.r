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

whalesighting_table <- as.data.frame(sighting_table) %>% 
  rename(c("Var1" = "year")) %>% 
  rename(c("Freq" = "count")) %>% 
  filter(year == "2012" | year == "2013" | year =="2014" | year == "2015" | year == "2016" | year == "2017" | year == "2018") %>% 
  mutate(category = "whales")

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




 

# Create my user interface

ui <- navbarPage("Navbar!",
                 tabPanel("Plot",
                          h1("Vessel and Sperm Whale Abundance off West Coast of Dominica 2012-2018"),
                          p("Sperm Whale Sightings and Vessel Categories"),
                    
                            sidebarLayout(
                            sidebarPanel(
                                         checkboxGroupInput(inputId = "category",
                                                            label = "Choose an Input",
                                                          choices = c("Sperm Whales" = "whales", "Cruise Ship" = "cruiseship","Merchant"= "merchant","High Speed Ferry"="high_speed_ferry")))
                            
                              
                            ,
                            mainPanel(
                              plotOutput(outputId = "vessel_cat_plot")
                            )
                          )
                 ),
                 tabPanel("Summary",
                          verbatimTextOutput("summary")
                 ),
                 tabPanel("More",
                          verbatimTextOutput("More")),
                 theme = shinytheme("flatly"))

                

server <- function(input, output) {
  
  vessel_category <- reactive({
    vessel_whale %>% 
      filter(category %in% input$category)
  })
  
  output$vessel_cat_plot <- renderPlot({
    ggplot(data = vessel_category(),
           aes(fill = category, x = year, y = count)) +
      geom_bar(position = "dodge",stat = "identity") +
      scale_fill_brewer(palette="Dark2") + 
      labs(x= "Year", y= "Number of Vessels") +
      theme_minimal()
    
 
      
  })
  
}


shinyApp(ui = ui, server = server)



