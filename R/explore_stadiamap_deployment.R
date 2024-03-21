
library(renv)
#library(swephR)
#library(lubridate)
#library(dplyr)
library(tigris)
#library(shiny)
#library(censusxy)
#library(scales)
library(ggplot2)
library(sf)
#library(glue)
#library(rsconnect)
library(ggmap)

renv::status()
renv::snapshot()

# https://forum.posit.co/t/set-api-key-for-shiny-app/31020
# https://github.com/STAT545-UBC/Discussion/issues/301


#stadiamap set api----
apikey <- "[STADIA MAP API KEY HERE]"
register_stadiamaps(key = apikey, write = FALSE)

us.states <- tigris::states(T) %>%
  .[.$STUSPS %in% state.abb,] %>%
  .[!.$STUSPS %in% c("HI", "AK"),]



uss.bbox <- sf::st_bbox(us.states)
names(uss.bbox) <- c("left", "bottom", "right", "top")

edg.stamen <- get_stadiamap(bbox = uss.bbox, 
                            zoom = 4, 
                            maptype = "stamen_toner_lite",#"stamen_terrain",                            
                            
                            crop = T, 
                            color = "color",
                            force = T,
                            size = 1);ggmap(edg.stamen)
