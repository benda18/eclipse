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
start.date <- Sys.Date()

#set.seed(3481851)

var.lon <- runif(1, -180, 180)
var.lat <- runif(1,   -90,  90)

# # data import----
# if(!"earth.coast" %in% ls()){
#   earth.coast <- ne_coastline(110) 
# }
# 
# # map----
# basemap <- ggplot() + 
#   geom_sf(data = earth.coast) +
#   geom_point(aes(x = var.lon, y = var.lat), 
#              color = "white", fill = "red", 
#              shape = 23, size =4) +
#   coord_sf()
# 
# print(basemap)

# work----

a.date.ju <- swephR::swe_utc_to_jd(year = year(start.date), 
                                   month = lubridate::month(start.date), 
                                   day   = mday(start.date), 
                                   houri = 0, 
                                   min   = 30, 
                                   sec   = 0, 
                                   gregflag = 1)$dret[2]

out.df       <- NULL
is_total_ecl <- F
n <- 0
while(!is_total_ecl){
  n <- n + 1
  if(n > 1000){
    stop("broken")
  }
  
  if(n > 1){
    a.date.ju <- when_next$tret[1]+1
  }
  
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
  
  # is partial or total? 
  
  if(n == 1 | 
     ifelse(temp.nextobs < 1, 
            "partial", "total") == "total"){
    out.df <- rbind(out.df, 
                    data.frame(from_date = start.date,
                               nth_ecl   = n,
                               lon = var.lon,
                               lat = var.lat,
                               ecl_date = temp.nextdate, 
                               ecl_obsc = temp.nextobs, 
                               ecl_type = ifelse(temp.nextobs < 1, 
                                                 "partial", "total")))
  }
  
  if(ifelse(temp.nextobs < 1, 
            "partial", "total") == "total"){
    is_total_ecl <- T
  }
  
}

out.df

