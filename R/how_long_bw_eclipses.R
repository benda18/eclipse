
library(renv)
library(swephR)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tigris)
library(readr)
library(data.table)

renv::snapshot()
renv::status()
rm(list=ls());cat('\f')

# funs----
get_xy <- function(bcdf_row){
  # longitude
  c(x = runif(n = 1, 
              min = bcdf_row$xmin, 
              max = bcdf_row$xmax), 
    y = runif(n = 1, 
              min = bcdf_row$ymin, 
              max = bcdf_row$ymax), 
    z = 10)
  
  # latitude
}

# how long between total eclipses?  
some.states <- state.abb[!state.abb %in% c("HI", "AK")]
usa.counties <- tigris::counties(state = some.states, cb = T)

bbox.co.df <- NULL
for(i in 1:nrow(usa.counties)){
  temp.bbox <- sf::st_bbox(usa.counties[i,])
  bbox.co.df <- rbind(bbox.co.df, 
                      as_tibble(
                        data.frame(st = usa.counties[i,]$STATE_NAME, 
                                   co = usa.counties[i,]$NAMELSAD, 
                                   xmin = temp.bbox["xmin"], 
                                   xmax = temp.bbox["xmax"], 
                                   ymin = temp.bbox["ymin"], 
                                   ymax = temp.bbox["ymax"])
                      )
  )
  rm(temp.bbox)
  
}

gc()

temp.eclinfo <- swephR::swe_sol_eclipse_when_loc(jd_start  = swephR::swe_utc_to_jd(2024, 4, 8, 15+4, 0, 0, 1)$dret[2], 
                                                 ephe_flag = 4, 
                                                 geopos    = get_xy(bbox.co.df[sample(1:nrow(bbox.co.df),1),]), 
                                                 backward  = F)

next.ecl <- temp.eclinfo$tret[1] %>% 
  swephR::swe_jdet_to_utc(., 1) %>%
  paste(., sep = "-", collapse = "-") %>%
  ymd_hms() %>%
  with_tz(., tzone = "America/New_York")
next.obs <- temp.eclinfo$attr[1]

