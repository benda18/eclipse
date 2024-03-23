library(swephR)
library(lubridate)
library(dplyr)
#library(tigris)
library(censusxy)
library(scales)
library(ggplot2)
library(sf)
library(renv)
#library(rnaturalearth)
#library(rnaturalearthdata)


getwd()

renv::snapshot()
renv::status()
rm(list=ls()[ls() != "earth.coast"]);cat('\f')


# vars----
start.date <- ymd(20240409)

get.addr <- censusxy::cxy_oneline(address = "7318 overland park court, west chester, oh")

var.lon <- unlist(unname(get.addr["coordinates.x"]))
var.lat <- unlist(unname(get.addr["coordinates.y"]))

a.date.ju <- swephR::swe_utc_to_jd(year = year(start.date), 
                                   month = lubridate::month(start.date), 
                                   day   = mday(start.date), 
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

temp.nextobs <- when_next$attr[1] # p

temp.dur <- ymd_hms(paste(swephR::swe_jdet_to_utc(when_next$tret[5], 1), sep = "-", collapse = "-")) -
  ymd_hms(paste(swephR::swe_jdet_to_utc(when_next$tret[2], 1), sep = "-", collapse = "-"))


