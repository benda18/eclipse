library(swephR)
library(lubridate)
library(dplyr)
#library(tigris)
library(censusxy)
library(scales)
library(ggplot2)
library(sf)
library(renv)
library(glue)
#library(rsconnect)


#renv::snapshot()
#renv::status()
rm(list=ls());cat('\f')

# funs----
eclipsewise_url <- function(ecl_date = ymd(20780511)){
  require(glue)
  require(lubridate)
  w.year  <- year(ecl_date)
  w.month <- as.character(lubridate::month(ecl_date, label = F))
  w.month <- ifelse(nchar(w.month) == 1,
                    paste("0", w.month, sep = "", collapse = ""), 
                    w.month)
  w.mday  <- mday(ecl_date)
  w.mday  <- ifelse(nchar(w.mday) == 1, 
                    paste("0", w.mday, sep = "", collapse = ""), 
                    w.mday)
  w.cenA  <- floor(w.year/100)*100+1
  w.cenB  <- w.cenA + 99
  list(c(partial = glue("https://eclipsewise.com/solar/SEping/{w.cenA}-{w.cenB}/SE{w.year}-{w.month}-{w.mday}P.gif"), 
         annular = glue("https://eclipsewise.com/solar/SEping/{w.cenA}-{w.cenB}/SE{w.year}-{w.month}-{w.mday}A.gif"), 
         total   = glue("https://eclipsewise.com/solar/SEping/{w.cenA}-{w.cenB}/SE{w.year}-{w.month}-{w.mday}T.gif")))
  
  
}
eclipsewise_url(ymd(20240408))


wiki_url <- function(ecl_date = ymd(20780511)){
  require(glue)
  require(lubridate)
  w.year  <- year(ecl_date)
  w.month <- as.character(lubridate::month(ecl_date, label = T, abbr = F))
  w.mday  <- mday(ecl_date)
  glue("https://en.wikipedia.org/wiki/Solar_eclipse_of_{w.month}_{w.mday},_{w.year}")
}

wiki_url()

# vars----
the.addr        <- sample(x = c("1600 Pennsylvania Ave, Washington, DC",      
                                "1060 W Addison, Chicago IL",               
                                "1 Bear Valley Rd, Point Reyes Station, CA",  
                                "250 E Franklin St, Chapel Hill, NC",          
                                "100 Joe Nuxhall Wy, Cincinnati, OH",        
                                "281 W Lane Ave, Columbus, OH",               
                                "300 Alamo Plaza, San Antonio, TX",           
                                "2634 Main St, Lake Placid, NY",             
                                "1047 Main St, Buffalo, NY" ,                 
                                "2610 University Cir, Cincinnati, OH",     
                                "3159 W 11th St, Cleveland, OH",              
                                "4001 W 2nd St, Roswell, NM",              
                                "926 E McLemore Ave, Memphis, TN",           
                                "369 Central Ave, Hot Springs, AR",         
                                "4790 W 16th St, Indianapolis, IN"), 
                          size = 1)#
start.date      <- ymd(20240409)
min_obsc <- 1 # will location be in path of totality?

# do work----
get.addr <- censusxy::cxy_oneline(address = the.addr)

var.lon <- unlist(unname(get.addr["coordinates.x"])) # runif(1, -180,180) 
var.lat <- unlist(unname(get.addr["coordinates.y"])) # runif(1, -90, 90)  

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
  
  temp.nextobs <- max(when_next$attr[c(3)]) # p
  
  log.ecls <- rbind(log.ecls, 
                    data.frame(n        = n, 
                               address  = the.addr,
                               date     = temp.nextdate,
                               strfdate = strftime(x = temp.nextdate, format = "%b %d, %Y"),
                               jdate    = NA,
                               ecl_type = NA,
                               obsc     = temp.nextobs))
  
  temp.utc <- last(log.ecls$date)
  temp.jd <- swe_utc_to_jd(year = year(temp.utc), 
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
try(next.obs)
log.ecls[sample(1:nrow(log.ecls), size = 3, replace = F),
         c("address", "date", "obsc")]

lapply(X = log.ecls[sample(1:nrow(log.ecls), size = 3, replace = F),]$date, 
       FUN = wiki_url)

lapply(X = log.ecls[sample(1:nrow(log.ecls), size = 3, replace = F),]$date, 
       FUN = eclipsewise_url)
