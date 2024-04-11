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


# renv::snapshot()

rm(list=ls());cat('\f')


data("countries110")

data(package = "rnaturalearthdata")

countries110$geounit
View(countries110)
countries110$scalerank %>% table

countries110$continent

ggplot() + 
  geom_sf(data = countries110[countries110$continent %in% 
                                c("North America", 
                                  "South America"),], 
          aes(fill = continent))+
  theme(panel.background = element_rect(fill = "#9ce4ff"), 
        legend.position = "none")


sort(unique(countries110$geounit[countries110$continent == "Oceania"]) ) %>%
  paste(collapse = "\", \"") %>%
  cat()
