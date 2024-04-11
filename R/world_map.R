library(renv)
#library(shiny)
#library(jpeg)
library(swephR)
library(lubridate)
library(dplyr)
library(tigris)
library(censusxy)
library(scales)
library(ggplot2)
library(sf)
library(glue)
#library(rsconnect)
#library(qrcode)
library(rnaturalearthdata) # for map of world
library(maps)



# renv::snapshot()

rm(list=ls());cat('\f')


data("countries110")

data(package = "rnaturalearthdata")

countries110$geounit
View(countries110)
countries110$scalerank %>% table

countries110$continent




sort(unique(countries110$geounit[countries110$continent == "Europe"]) ) %>%
  paste(collapse = "\", \"") %>%
  cat()

unique(countries110$subregion)
sort(unique(countries110$geounit[countries110$subregion == "Melanesia"]) ) %>%
  paste(collapse = "\", \"") %>%
  cat()


countries110[countries110$geounit == "France",] %>% t()


maps::world.cities

ggplot() + 
  geom_sf(data = countries110, 
          aes(fill = continent)) +
  # geom_point(data = slice_max(world.cities,
  #                             order_by = pop, 
  #                             prop = 0.0125), 
  #            aes(x = long, y = lat)) +
  theme(panel.background = element_rect(fill = "#9ce4ff"), 
        legend.position = "none")


# world map continent edits----

countries110 %>%
  
  ggplot(data = .) + 
  geom_sf(aes(fill = continent)) +
  # geom_point(data = slice_max(world.cities,
  #                             order_by = pop, 
  #                             prop = 0.0125), 
  #            aes(x = long, y = lat)) +
  theme(panel.background = element_rect(fill = "#9ce4ff"), 
        legend.position = "none")
