library(renv)
#library(shiny)
#library(jpeg)
library(swephR)
library(lubridate)
library(dplyr)
library(tigris)
library(censusxy)
library(scales)
library(ggplot2)
library(sf)
library(glue)
#library(rsconnect)
#library(qrcode)
library(rnaturalearthdata) # for map of world

#renv::snapshot()

rm(list=ls());cat('\f');gc()

# RESOURCES----
# www.astro.com/swisseph/swephprg.htm

# FUNS----


# UNIVERSAL VARIABLES----
lon.in     <- runif(-180,180,n=1)
lat.in     <- runif(-90, 90, n=1)
greg_dt.in <- Sys.time()
loctz.in   <- "America/New_York"

# Transformations----
greg_utcdt.in <- greg_dt.in |> with_tz(tzone = "UTC")

jul_utcdt.in  <- swephR::swe_utc_to_jd(year = year(greg_utcdt.in), 
                                    month = lubridate::month(greg_utcdt.in), 
                                    day   = mday(greg_utcdt.in), 
                                    houri = hour(greg_utcdt.in), 
                                    min   = minute(greg_utcdt.in), 
                                    sec   = second(greg_utcdt.in), 
                                    gregflag = 1 #SE$GREG_CAL = 1
                                    )$dret[2] # dret[2] = UTC, [1] = ET (not sure what et means)


# SWEPHR FUNS----
# find the next eclipse for a given geographic position;
when_next <- swe_sol_eclipse_when_loc(jd_start  = jul_utcdt.in, 
                         ephe_flag = 4, 
                         geopos    = c(x = lon.in, 
                                       y = lat.in, 
                                       z = 10), 
                         backward  = FALSE) 

# find the next eclipse globally (including type of eclipse);
ecl_total   <- swe_sol_eclipse_when_glob(jd_start  = jul_utcdt.in,
                                         ephe_flag = 4, 
                                         ifltype   = SE$ECL_TOTAL, 
                                         backward  = FALSE)
ecl_annular <- swe_sol_eclipse_when_glob(jd_start  = jul_utcdt.in,
                                         ephe_flag = 4, 
                                         ifltype   = SE$ECL_ANNULAR,
                                         backward  = FALSE)
ecl_partial <- swe_sol_eclipse_when_glob(jd_start  = jul_utcdt.in,
                                         ephe_flag = 4, 
                                         ifltype   = SE$ECL_PARTIAL,
                                         backward  = FALSE)
ecl_hybrid  <- swe_sol_eclipse_when_glob(jd_start  = jul_utcdt.in,
                                         ephe_flag = 4, 
                                         ifltype   = SE$ECL_ANNULAR_TOTAL,
                                         backward  = FALSE)

ecl_type <- c("Total Eclipse", "Annular", 
              "Partial", "Hybrid")[which(abs(c(ecl_total$tret[2] - when_next$tret[2],
                                               ecl_annular$tret[2] - when_next$tret[2],
                                               ecl_partial$tret[2] - when_next$tret[2],
                                               ecl_hybrid$tret[2] - when_next$tret[2])) == 
                                           min(abs(c(ecl_total$tret[2] - when_next$tret[2],
                                                     ecl_annular$tret[2] - when_next$tret[2],
                                                     ecl_partial$tret[2] - when_next$tret[2],
                                                     ecl_hybrid$tret[2] - when_next$tret[2]))))]

# compute the geographic location of a solar eclipse for a given tjd;

jul_utc_ecl.begin <- swe_sol_eclipse_when_glob(jd_start  = jul_utcdt.in,
                                             ephe_flag = 4, 
                                             ifltype   = 0, # 0 = any eclipse 
                                             backward  = FALSE)$tret[c(3:4)]


swe_sol_eclipse_where(jd_ut     = min(jul_utc_ecl.begin), 
                      ephe_flag = 4) 
swe_sol_eclipse_where(jd_ut     = max(jul_utc_ecl.begin), 
                      ephe_flag = 4) 

# compute attributes of a solar eclipse for a given tjd, geographic longitude,
# latitude and height.
swe_sol_eclipse_how(jd_ut     = jul_utcdt.in, 
                    ephe_flag = 4, 
                    geopos    = c(x = lon.in, 
                                  y = lat.in, 
                                  z = 10)) 

# find the next lunar eclipse for a given geographic position;
swe_lun_eclipse_when_loc(jd_start  = jul_utcdt.in, 
                         ephe_flag = 4, 
                         geopos    = c(x = lon.in, 
                                       y = lat.in, 
                                       z = 10), 
                         backward  = FALSE) 

# find the next lunar eclipse;
swe_lun_eclipse_when(jd_start  = jul_utcdt.in,
                     ephe_flag = 4, 
                     ifltype   = NA, # SE_ECL_TOTAL, SE_ECL_PARTIAL, or SE_ECL_PENUMBRAL
                     backward  = FALSE) 

# compute the attributes of a lunar eclipse for a given tjd.
swe_lun_eclipse_how(jd_ut     = jul_utcdt.in, 
                    ephe_flag = 4,
                    geopos    = c(x = lon.in, 
                                  y = lat.in, 
                                  z = 10)) 

