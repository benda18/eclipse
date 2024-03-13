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



# https://github.com/chris-prener/censusxy?tab=readme-ov-file

# cite: censusxy is described in a 2021 paper in Transactions in GIS by Chris
# and Branson - please cite the paper if you use censusxy in your work!


rm(list=ls());cat('\f');gc()

# SET VARS----

# cxy.addr <- censusxy::cxy_oneline(address = "1060 W Addison St, Chicago, IL") 
# 
# 
# cxy.addr$coordinates.x
# cxy.addr$coordinates.y
# cxy.addr$matchedAddress



lon_in <- -78.91775 #-81.44067000
lat_in <-  36.04909 #41.24006000

# ECLIPSE MATH----

# set date_time of eclipse (ideally before solar eclipse begins)----

greg_dt.local <- ymd_hm("2024-04-07 08:30AM", tz = "America/New_York")
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

ewl_out$tret <- ewl_out$tret[1:5]



ewl_out$attr <- ewl_out$attr[1+c(2)]
#names(ewl_out$attr) <- c("pct_obscuration")

# out_times----
out.times <- data.frame(time_val.jul     = ewl_out$tret, 
                        local_time       = NA)

for(i in 1:nrow(out.times)){
  out.times$local_time[i] <- swephR::swe_jdet_to_utc(jd_et = ewl_out$tret[i], 
                                                     gregflag = 1) %>%
    paste(., 
          sep = "-", 
          collapse = "-") %>%
    ymd_hms() %>%
    with_tz(., tz.local) %>%
    strftime(., 
             format = "%I:%M:%S%p %Z", 
             tz = tz.local)
}

out.times <- out.times[order(out.times$local_time),]
out.times$eclipse_event <- c("begin_partial_eclipse", 
                             "begin_total_eclipse", 
                             "maximum_total_eclipse", 
                             "end_total_eclipse", 
                             "end_partial_eclipse")
rownames(out.times) <- 1:nrow(out.times)
out.times <- out.times[,c("eclipse_event", "local_time")]

# out_attributes----
out.attr <- data.frame(longitude = lon_in, 
                       latitude  = lat_in,
                       total_ecl_at_loc = ewl_out$attr > 1)
rownames(out.attr) <- 1:nrow(out.attr)
out.attr
out.times


# BASEMAP----
ec.states <- c("IN", "OH", "KY",
               "TX", "OK", "AR", "MO",
               "IL", "MI", "PA", "NY",
                "VT", "NH", "ME",
              "TN","WV", "MA",
"RI", "CT", "WI", "LA",
"MS", "KS", "IA",
"AL", "GA", "DE", "NJ", "VA",
"SC", "NC", "MD", "FL", "MN", 
"IA", "ND", "SD", "AZ")

state.abb

# show_interstates <- c(75,71,70,90,80, 
#                       64, 74, 24, 
#                       69, 65)
oki.states <- tigris::states(T) %>%
  .[.$STUSPS %in% state.abb,] %>%
  .[!.$STUSPS %in% c("AK", "HI"),]
gc()

# pr.usa <- tigris::primary_roads(2021) %>%
#   .[grepl("^I- \\d{1,2}$", 
#           .$FULLNAME, ignore.case = F),]
gc()
# oki.counties <- tigris::counties(state = c("OH", "IN", "KY"))
gc()

# oki.st_co <- data.frame(st = oki.counties$STATEFP, 
#                         co = oki.counties$COUNTYFP)

#rm(oki.counties)

#oh.counties <- oki.counties$NAME[oki.counties$STATEFP == "39"] %>% sort()
#ky.counties <- oki.counties$NAME[oki.counties$STATEFP == "21"] %>% sort()
#in.counties <- oki.counties$NAME[oki.counties$STATEFP == "18"] %>% sort()
#rm(oki.counties)

gc()



# 
# if(!"oh.ir" %in% ls()){
#   oh.irA <- tigris::roads(state = "OH", county = oh.counties[1:10]) %>%
#     .[grepl("^I- \\d{1,3}$", .$FULLNAME, ignore.case = F),]
#   
# }


ggplot() + 
  geom_sf(data = oki.states)#+
  #geom_sf(data = oh.ir)


getwd(
  
)

#saveRDS(object = oki.states, file = "~/R/play/eclipse/shiny_eclipse_timer/okistates.rds")
