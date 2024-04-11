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

rm(list=ls());cat('\f');gc()

# RESOURCES----
# www.astro.com/swisseph/swephprg.htm

# FUNS----


# SWEPHR FUNS----
# find the next eclipse for a given geographic position;
swe_sol_eclipse_when_loc(jd_start  = NA, 
                         ephe_flag = 4, 
                         geopos    = c(x = NA, 
                                       y = NA, 
                                       z = 10), 
                         backward  = FALSE) 

# find the next eclipse globally;
swe_sol_eclipse_when_glob(jd_start  = NA,
                          ephe_flag = 4, 
                          ifltype   = NA, # SE$ECL_TOTAL, SE$ECL_ANNULAR, SE$ECL_PARTIAL, or SE$ECL_ANNULAR_TOTAL (hybrid)
                          backward  = FALSE) 

c("Total Eclipse", "Annular", 
  "Partial", "Hybrid")[which(abs(c(ecl_total$tret[2] - when_next$tret[2],
                                   ecl_annular$tret[2] - when_next$tret[2],
                                   ecl_partial$tret[2] - when_next$tret[2],
                                   ecl_hybrid$tret[2] - when_next$tret[2])) == 
                               min(abs(c(ecl_total$tret[2] - when_next$tret[2],
                                         ecl_annular$tret[2] - when_next$tret[2],
                                         ecl_partial$tret[2] - when_next$tret[2],
                                         ecl_hybrid$tret[2] - when_next$tret[2]))))]


# compute the geographic location of a solar eclipse for a given tjd;
swe_sol_eclipse_where() 

# compute attributes of a solar eclipse for a given tjd, geographic longitude,
# latitude and height.
swe_sol_eclipse_how() 

# find the next lunar eclipse for a given geographic position;
swe_lun_eclipse_when_loc(tjd...) 

# find the next lunar eclipse;
swe_lun_eclipse_when(tjd...) 

# compute the attributes of a lunar eclipse for a given tjd.
swe_lun_eclipse_how() 

