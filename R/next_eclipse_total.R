library(swephR)
library(lubridate)
library(dplyr)
#library(tigris)
library(censusxy)
library(scales)
library(ggplot2)
#library(sf)
library(renv)
library(glue)
#library(rsconnect)


#renv::snapshot()
#renv::status()
rm(list=ls());cat('\f')

# funs----

# vars----
the.addr        <- sample(x = c("1600 Pennsylvania Ave, Washington, DC",      
                                "1060 W Addison, Chicago IL",               
                                "2634 Main St, Lake Placid, NY",             
                                "1047 Main St, Buffalo, NY" ,                 
                                "2610 University Cir, Cincinnati, OH",     
                                "3159 W 11th St, Cleveland, OH",              
                                "4001 W 2nd St, Roswell, NM",              
                                "926 E McLemore Ave, Memphis, TN",           
                                "369 Central Ave, Hot Springs, AR",         
                                "4790 W 16th St, Indianapolis, IN"), 
                          size = 1)#
start.date <- ymd(20240409)
min_obsc   <- 1 

# do work----
get.addr <- censusxy::cxy_oneline(address = the.addr)

var.lon <- unlist(unname(get.addr["coordinates.x"])) # runif(1, -180,180) 
var.lat <- unlist(unname(get.addr["coordinates.y"])) # runif(1, -90, 90)  

####
is_totality <- F
n <- 0
while(!is_totality & year(start.date) < 3001){
  n <- n + 1
  if(n > 2000){
    stop("too many searches - ERROR")
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
  
  temp.nextdate <- ymd_hms(paste(swephR::swe_jdet_to_utc(when_next$tret[1], 1), 
                                 sep = "-", collapse = "-"))
  
  temp.nextobs <- when_next$attr[c(3)] 
  
  if(temp.nextobs >= min_obsc){
    is_totality <- T
    next.obs <- temp.nextobs
    start.date <- as_date(temp.nextdate)
  }else{
    start.date <- as_date(temp.nextdate) + days(2)
    next.obs <- temp.nextobs
  }
}

# do next----
if(temp.nextobs < 1 & 
   year(start.date) > 3000){
  next.total.eclipse <- "Sometime after the year 3000"
}else{
  next.total.eclipse <-  strftime(start.date, format = "%B %d, %Y")
}

next.total.eclipse
next.obs
