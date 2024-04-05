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



# vars----
the.addr        <- "1600 Pennsylvania Ave, Washington, DC"
start.date      <- ymd(20240409)
min_obsc        <- 1 # will location be in path of totality?

# do work----
get.addr <- censusxy::cxy_oneline(address = the.addr)

var.lon <- unlist(unname(get.addr["coordinates.x"])) # runif(1, -180,180) 
var.lat <- unlist(unname(get.addr["coordinates.y"])) # runif(1, -90, 90)  

####
is_totality <- F
n <- 0

log.ecls <- NULL

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
  
  temp.nextobs <- when_next$attr[c(3)] # p
  
  log.ecls <- rbind(log.ecls,
                    data.frame(#n        = n,
                      #address  = the.addr,
                      #date     = temp.nextdate,
                      date = strftime(x = temp.nextdate, 
                                      format = "%b %d, %Y", 
                                      tz = "America/New_York"),
                      #jdate    = NA,
                      #ecl_type = NA,
                      pct_obscured = temp.nextobs, 
                      url = NA))

  temp.utc <- temp.nextdate
  temp.jd  <- swe_utc_to_jd(year = year(temp.utc),
                           month = lubridate::month(x = temp.utc, label = F),
                           day = mday(temp.utc),
                           houri = hour(temp.utc),
                           min = minute(temp.utc),
                           sec = second(temp.utc),
                           gregflag = 1)$dret |>
    as.integer() |>
    unique()
  
  if(temp.nextobs >= min_obsc){
    is_totality <- T
    next.obs <- temp.nextobs
    start.date <- as_date(temp.nextdate)
  }else{
    start.date <- as_date(temp.nextdate) + days(2)
    next.obs <- temp.nextobs
  }
}

# # do next----
# if(temp.nextobs < 1 & 
#    year(start.date) > 3000){
#   next.total.eclipse <- "Sometime after the year 3000"
# }else{
#   next.total.eclipse <-  strftime(start.date, format = "%B %d, %Y")
# }




log.ecls$pct_obscured <- floor(round(log.ecls$pct_obscured *100, digits = 1))

log.ecls$pct_obscured <- ifelse(test = log.ecls$pct_obscured >= 100, 
                                yes = "<<<TOTALITY>>>" , 
                                no = log.ecls$pct_obscured)

log.ecls

# log.ecls[sample(1:nrow(log.ecls), size = 3, replace = F),
#          c("address", "date", "pct_obscured")]
# 
# lapply(X = log.ecls[sample(1:nrow(log.ecls), size = 3, replace = F),]$date,
#        FUN = wiki_url)
# 
# lapply(X = log.ecls[sample(1:nrow(log.ecls), size = 3, replace = F),]$date,
#        FUN = eclipsewise_url)
