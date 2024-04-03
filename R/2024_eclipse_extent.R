library(renv)
library(swephR)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tigris)
library(readr)

renv::snapshot()
renv::status()

rm(list=ls());cat('\f');gc()

# Funs----
# to do: fix so you get non-total occlusion info returned. 
get_next_total_eclipse <- function(jdate, 
                                   lonlat = c("x"=0,"y"=0)){
  
  is.te <- F
  while(!is.te){
    # check date and location 
    temp.when.loc <- swe_sol_eclipse_when_loc(jd_start  = jdate, 
                                              ephe_flag = 4, 
                                              geopos    = c(lonlat,10), 
                                              backward  = F)
    # is total eclipse? 
    if(temp.when.loc$attr[9] == temp.when.loc$attr[2]){
      # yes
      #print("is total eclipse")
      dt.eclipse.max <- temp.when.loc$tret[1] %>%
        swephR::swe_jdut1_to_utc(.,1)
      dt.eclipse.max <- ymd_hms(paste(dt.eclipse.max, sep = "", collapse = "-"))
      # finish while statement logic
      is.te <- T
    }else{
      # no
      #print("is not total eclipse")
      # increment jdate
      jdate <- temp.when.loc$tret[1] + 1
    }
    
  }
  return(dt.eclipse.max)
}

jdate1 <- swe_sol_eclipse_when_glob(jd_start = 2460405,
                                   4, 0, F)$tret[1]
lonlat1 <- c(-81,42)

get_next_total_eclipse(jdate1, lonlat1)




# Vars----
by_deg <- 10
jdate.start <- swephR::swe_utc_to_jd(2024,04,04, #2024,04,5,
                                     4,30,0,1)$dret[1]


april.grid <- expand.grid(lon = seq(-180,180,by = by_deg), 
                          lat = seq(-90, 90, by = by_deg)) %>%
  as_tibble() 



