library(swephR)
library(lubridate)
library(dplyr)
#library(tigris)
library(censusxy)
library(scales)
library(ggplot2)
library(sf)
library(renv)
#library(rsconnect)


#renv::snapshot()
#renv::status()
rm(list=ls());cat('\f')


# vars----
the.addr   <- "1210 laurel meadows dr, durham nc"#"504 N queen st, durham, nc"
start.date <- mdy("april 9, 2024")#ymd(20240409)
obs.gte    <- 0.65 # obscuration greater than or equal to (percent)

# do work----
get.addr <- censusxy::cxy_oneline(address = the.addr)

var.lon <- unlist(unname(get.addr["coordinates.x"]))
var.lat <- unlist(unname(get.addr["coordinates.y"]))

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
  
  temp.nextobs <- max(when_next$attr[c(1,3)]) # p
  
  # check to see if total eclipse or partial
  ecl_type <- ifelse(when_next$attr[2] >= 1, "total", "partial")
  
  
  if(temp.nextobs >= obs.gte){
    is_totality <- T
    next.obs <- temp.nextobs
    start.date <- as_date(temp.nextdate)
  }else{
    start.date <- as_date(temp.nextdate) + days(2)
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
