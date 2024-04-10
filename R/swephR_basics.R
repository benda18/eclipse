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

# Solar eclipses:

# find the next eclipse for a given geographic position;
swe_sol_eclipse_when_loc(tjd...) 

# find the next eclipse globally;
swe_sol_eclipse_when_glob(tjd...) 

# compute the geographic location of a solar eclipse for a given tjd;
swe_sol_eclipse_where() 

# compute attributes of a solar eclipse for a given tjd, geographic longitude,
# latitude and height.
swe_sol_eclipse_how() 

# Lunar eclipses:

# find the next lunar eclipse for a given geographic position;
swe_lun_eclipse_when_loc(tjd...) 

# find the next lunar eclipse;
swe_lun_eclipse_when(tjd...) 

# compute the attributes of a lunar eclipse for a given tjd.
swe_lun_eclipse_how() 

