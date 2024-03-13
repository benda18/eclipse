library(swephR)
library(lubridate)
library(dplyr)
#library(tigris)
library(censusxy)
library(scales)
library(ggplot2)
library(sf)
library(renv)
library(rnaturalearth)
library(rnaturalearthdata)


getwd()

renv::snapshot()
renv::status()
rm(list=ls()[ls() != "earth.coast"]);cat('\f')


# vars----
a.date <- Sys.Date()

#a.num <- runif(1, min = 0, max = 10000000) %>% floor()
set.seed(3481851)

var.lon <- runif(1, -180, 180)
var.lat <- runif(1,   -90,  90)

# data import----
if(!"earth.coast" %in% ls()){
  earth.coast <- ne_coastline(110) 
}

# map----
basemap <- ggplot() + 
  geom_sf(data = earth.coast) +
  geom_point(aes(x = var.lon, y = var.lat), 
             color = "white", fill = "red", 
             shape = 23, size =4) +
  coord_sf()

print(basemap)

# work----

a.date.ju <- swephR::swe_utc_to_jd(year = year(a.date), 
                                   month = lubridate::month(a.date), 
                                   day   = mday(a.date), 
                                   houri = 0, 
                                   min   = 30, 
                                   sec   = 0, 
                                   gregflag = 1)$dret[2]

when_next <- swe_sol_eclipse_when_loc(jd_start = a.date.ju, 
                                      ephe_flag = 4, 
                                      geopos = c(x = var.lon, 
                                                 y = var.lat, 
                                                 z = 10), 
                                      backward = F)

temp.nextdate <- when_next$tret[1] %>% # time of maximum eclipse 
  swephR::swe_jdet_to_utc(., 1) %>%
  paste(., sep = "-", collapse = "-") %>%
  ymd_hms()

temp.nextobs <- when_next$attr[1] # pct obscred


data.frame(ecl_date = temp.nextdate, 
           ecl_obsc = temp.nextobs)
