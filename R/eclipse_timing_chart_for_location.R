library(renv)
library(swephR)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tigris)
#library(tidycensus)
library(censusxy)
#library(readr)
#library(data.table)
#library(shiny)
library(rsconnect)

renv::snapshot()
renv::status()

rm(list=ls());cat('\f');gc()

# vars----
lon  <- -78.91775 #-81.44067000
lat  <-  36.04909 #41.24006000
time_NY <- ymd_hm("2024-04-07 08:30AM", tz = "America/New_York")

# function----
ec_sched <- function(lon_in, lat_in, time_ny){
  # time logic
  
  if(!is.POSIXct(time_ny)){
    stop("'time_ny' input must be of class 'POSIXct' or 'POSIXt'.
         Use lubridate::ymd_hms() or similar, and ensure timezone 
         set to America/NewYork.")
  }
  
  greg_dt.local <- time_ny
  tz.local      <- tz(greg_dt.local)
  
  # do the time conversions----
  # convert to utc
  greg_dt.utc <- with_tz(greg_dt.local, tz = "UTC")
  jul_dt.utc  <- swephR::swe_julday(year  = year(greg_dt.utc), 
                                    month = lubridate::month(greg_dt.utc, label = F), 
                                    day   = mday(greg_dt.utc), 
                                    hourd = hour(greg_dt.utc) + 
                                      (minute(greg_dt.utc)/60) + 
                                      (second(greg_dt.utc)/60/60), 
                                    gregflag = 1)
  
  ewl_out     <- swephR::swe_sol_eclipse_when_loc(jd_start  = jul_dt.utc, 
                                                  ephe_flag = 4, 
                                                  geopos    = c(x = lon_in, 
                                                                y = lat_in, 
                                                                z = 10), 
                                                  backward = F)
  
  # get start, max, end----
  ecl_times <- data.frame(st = with_tz(ymd_hms(paste(swe_jdet_to_utc(jd_et = ewl_out$tret[2], 
                                                                     gregflag = 1),
                                                     collapse = "-")), 
                                       tzone = "America/New_York"), 
                          mt = with_tz(ymd_hms(paste(swe_jdet_to_utc(jd_et = ewl_out$tret[1], 
                                                                     gregflag = 1),
                                                     collapse = "-")), 
                                       tzone = "America/New_York"), 
                          et = with_tz(ymd_hms(paste(swe_jdet_to_utc(jd_et = ewl_out$tret[5], 
                                                                     gregflag = 1),
                                                     collapse = "-")), 
                                       tzone = "America/New_York"))
  
  ecl_times2 <- as.vector(ecl_times)
  
  break_mins <- 10
  end_time <- ecl_times2$st 
  n        <- 0
  
  out.times <- ecl_times2 %>%
    unname() %>%
    unlist()
  
  while(end_time < ecl_times2$et){
    n <- n + 1
    if(n > 1000){
      stop("error - N")
    }
    
    
    end_time <- end_time %m+% minutes(break_mins)
    
    if(end_time < ecl_times2$et){
      out.times <- c(out.times, 
                     end_time)
    }
  }
  
  out.times <- out.times %>%
    unique() %>%
    sort() %>%
    as_datetime() %>% 
    with_tz(., tzone = "America/New_York") 
  
  # / start, max, end
  #return(out.times)
  
  #ecsched.times <- ec_sched(lon, lat, time_NY)
  ecsched.times <- out.times
  
  # get sun coverage----
  
  # convert time to julian
  ecsuncov <- NULL
  
  for(i in 1:length(ecsched.times)){
    i_time <-  swephR::swe_julday(year = year(with_tz(ecsched.times[i],"UTC")), 
                                  month = lubridate::month(with_tz(ecsched.times[i],"UTC")), 
                                  day = mday(with_tz(ecsched.times[i],"UTC")), 
                                  hourd = lubridate::hour(with_tz(ecsched.times[i],"UTC")) + 
                                    (lubridate::minute(with_tz(ecsched.times[i],"UTC"))/60), 
                                  gregflag = 1)
    
    ecsuncov <- c(ecsuncov, 
                  swephR::swe_sol_eclipse_how(jd_ut = i_time, 
                                              ephe_flag = 4, 
                                              geopos = c(x = lon, 
                                                         y = lat, 
                                                         z = 10))$attr[1])
  }
  
  return(data.frame(time = ecsched.times, 
             coverage = ecsuncov))
}

# do function----
ec_sched(lon, lat, time_NY)

