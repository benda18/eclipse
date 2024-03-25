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
rm(list=ls()[ls() != "get.addr"]);cat('\f')


# vars----

if(! "get.addr" %in% ls()){
  get.addr <- censusxy::cxy_oneline(address = "1447 newcastle rd, durham nc")
}

var.lon <- unlist(unname(get.addr["coordinates.x"]))
var.lat <- unlist(unname(get.addr["coordinates.y"]))

start.date <- ymd(20240409)
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
  
  
  if(temp.nextobs >= 0.5){
    is_totality <- T
  }else{
    start.date <- as_date(temp.nextdate) + days(2)
  }
}

if(temp.nextobs < 1 & 
   year(start.date) > 3000){
  next.total.eclipse <- "Sometime after the year 3000"
}else{
  next.total.eclipse <-  strftime(start.date, format = "%B %d, %Y")
}



# next lunar eclipse----
le_type <- function(mag_u){
  # total 
  tot_ecl <- mag_u >= 1
  # penumbral
  pen_ecl <- mag_u < 0
  # partial
  par_ecl <- !xor(tot_ecl,pen_ecl)
  
  out <- c("Total Lunar" = tot_ecl, 
           "Penumbral Lunar" = pen_ecl, 
           "Partial Lunar" = par_ecl)
  
  out <- out[out == T]
  out <- names(out)
  return(out)
}

start.date <- ymd(20240409)

if(! "get.addr" %in% ls()){
  get.addr <- censusxy::cxy_oneline(address = "1447 newcastle rd, durham nc")
}

var.lon <- unlist(unname(get.addr["coordinates.x"]))
var.lat <- unlist(unname(get.addr["coordinates.y"]))

a.date.ju <- swephR::swe_utc_to_jd(year = year(start.date), 
                                   month = lubridate::month(start.date), 
                                   day   = mday(start.date), 
                                   houri = 0, 
                                   min   = 30, 
                                   sec   = 0, 
                                   gregflag = 1)$dret[2]

when_lunar <- swephR::swe_lun_eclipse_when_loc(jd_start = a.date.ju, 
                                               ephe_flag = 4, 
                                               geopos = c(x = var.lon, 
                                                          y = var.lat, 
                                                          z = 10), 
                                               backward = F)

# https://www.astro.com/swisseph/swephprg.htm#_Toc112948992

next_lunar <- strftime(x = ymd_hms(paste(swephR::swe_jdet_to_utc(when_lunar$tret[1], 1), 
                                         sep = "-", collapse = "-")), 
                       format = "%B %d, %Y at %I:%M%p %Z")
app_alt_deg <- when_lunar$attr[7]

ecl_type <- le_type(when_lunar$attr[1])

data.frame(date_time = next_lunar, 
           type = ecl_type, 
           degrees_above_horizon = round(app_alt_deg,1))

#mag_u <- mag.umb  <- when_lunar$attr[1]






