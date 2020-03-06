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
library(sf)
library(tmap)

#Read in vessel and whale data
vessels <- read_csv("shiny_vessels.csv")
sightings <- read_csv("sightings.csv")

sightings <- sightings %>%
  mutate(parsedate = mdy_hm(datetime)) %>%
  mutate(year = lubridate::year(parsedate))

#wrangling for whale and vessel graph 
year_vessel <- vessels %>%
  mutate(year = year(timestamp))

whales_vessels <- bind_rows(sightings, year_vessel)

whales_vessels_sf <- st_as_sf(whales_vessels, coords = c("lon", "lat"), crs = 4326)

#creating whale and vessel map with tmap 
tmap_mode("view")

whale_vessel_map <- tm_shape(whales_vessels_sf) + 
  tm_dots(labels = "mmsi", col = "green", size = 0.02, alpha = 0.4) + 
  tm_shape(whales_vessels_sf) +
  tm_dots(labels = "total", col = "orange", size = 0.02, alpha = 0.4) +
  tm_basemap("Stamen.Terrain")

whale_vessel_map
  
