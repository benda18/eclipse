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

get.addr <- censusxy::cxy_oneline(address = "882 alnetta, cincinnati, oh")

var.lon <- unlist(unname(get.addr["coordinates.x"]))
var.lat <- unlist(unname(get.addr["coordinates.y"]))


is_totality <- F
n <- 0
while(!is_totality){
  n <- n + 1
  if(n > 5000){
    stop("too many searches - something wrong")
  }
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
  
  temp.nextobs <- max(when_next$attr[c(1,3)]) # p
  
  ecl_type <- ifelse(temp.nextobs >= 1, "total", "partial")
  
  if(ecl_type == "total"){
    is_totality <- T
  }else{
    start.date <- as_date(temp.nextdate) + days(2)
  }
  
  # temp.dur <- ymd_hms(paste(swephR::swe_jdet_to_utc(when_next$tret[5], 1), sep = "-", collapse = "-")) -
  #   ymd_hms(paste(swephR::swe_jdet_to_utc(when_next$tret[2], 1), sep = "-", collapse = "-"))
}






