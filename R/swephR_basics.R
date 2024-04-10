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


# swephR_functions----

# Solar eclipses:

# swe_sol_eclipse_when_loc(tjd...) finds the next eclipse for a given geographic
# position;

# swe_sol_eclipse_when_glob(tjd...) finds the next eclipse globally;

# swe_sol_eclipse_where() computes the geographic location of a solar eclipse
# for a given tjd;

# swe_sol_eclipse_how() computes attributes of a solar eclipse for a given tjd,
# geographic longitude, latitude and height.

# Lunar eclipses:

# swe_lun_eclipse_when_loc(tjd...) finds the next lunar eclipse for a given
# geographic position;

# swe_lun_eclipse_when(tjd...) finds the next lunar eclipse;

# swe_lun_eclipse_how() computes the attributes of a lunar eclipse for a given
# tjd.

