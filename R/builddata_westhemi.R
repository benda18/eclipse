library(renv)
library(swephR)
#library(lubridate)
library(dplyr)
library(ggplot2)
library(tigris)

renv::snapshot()
renv::status()
#renv::restore()


rm(list=ls());cat('\f')
getwd()

# Funs----
# fun_min2dec <- function(x, rnd = 2){
#   round(x/60,rnd)
# }

fun_eclpoly <- function(ec_where_in, ec_time_in){
  data.frame(time_jd = ec_time_in, 
             lon = ec_where_in$pathpos[1], 
             lat = ec_where_in$pathpos[2], 
             shadow_dia_km = ec_where_in$attr[4], 
             mag = ec_where_in$attr[9], 
             obs_pct = ec_where_in$attr[3])
}

#swephR::swe_sol_eclipse_when_loc()  # Find the next solar eclipse for a given geographic position.
#swephR::swe_sol_eclipse_when_glob() # Find the next solar eclipse on earth.
#swephR::swe_sol_eclipse_where()     # Compute the geographic position of a solar eclipse path.
#swephR::swe_sol_eclipse_how()       # Compute the attributes of a solar eclipse for a given time.

ec_dur <- swephR::swe_sol_eclipse_when_glob(jd_start = 2460400, 
                                            4, 
                                            SE$ECL_TOTAL+SE$ECL_CENTRAL+SE$ECL_NONCENTRAL,
                                            FALSE)$tret[c(3,4)]

# grid.jtimes <- expand.grid(year  = 2024, 
#                            month = 4, 
#                            day   = 8, 
#                            hour  = 6:20, 
#                            min   = fun_min2dec(seq(0,59, by = 5))) %>%
#   .[order(.$hour, .$min),] %>% 
#   as_tibble() %>%
#   mutate(., 
#          jdate = NA)
# 
# 
# for(i in 1:nrow(grid.jtimes)){
#   grid.jtimes$jdate[i] <- swephR::swe_julday(year  = grid.jtimes$year[i], 
#                                              month = grid.jtimes$month[i], 
#                                              day   = grid.jtimes$day[i],
#                                              hourd = grid.jtimes$hour[i] + grid.jtimes$min[i] + 4,
#                                              SE$GREG_CAL)
# }

in.jtimes <- seq(min(ec_dur), 
                   max(ec_dur), 
                   length.out = 100)

grid.jtimes <- NULL

for(i in 1:length(in.jtimes)){
  temp <- swe_sol_eclipse_where(jd_ut = in.jtimes[i], 
                                SE$FLG_MOSEPH)
  grid.jtimes <- rbind(grid.jtimes, 
                       fun_eclpoly(temp, in.jtimes[i]))
}

plot(grid.jtimes)

# temp.dt_utc <- c(year  = 2024, 
#                  month = 4, 
#                  day   = 8, 
#                  hour  = 15.3 + 4)
#
# temp.jd <- swephR::swe_julday(year = temp.dt_utc["year"], 
#                               month = temp.dt_utc["month"], 
#                               day   = temp.dt_utc["day"], 
#                               hourd = temp.dt_utc["hour"], 
#                               SE$GREG_CAL)
# 
# temp.jd - as.integer(temp.jd)



# scales::comma(ec_dur, accuracy = 0.00000001)
# 
# next.ec$tret %>% diff %>% scales::comma(., accuracy = 0.00001)
#   
# temp.ec.where <- swe_sol_eclipse_where(jd_ut = temp.jd,
#                                        ephe_flag = SE$FLG_MOSEPH)
# 
# # ec_where_in <- temp.ec.where
# # ec_time_in  <- temp.jd
# 
# 
# 
# fun_eclpoly(temp.ec.where, temp.jd)
# 
# ec_where_pp.attr <- c("central line - lon", 
#                       "central line - lat", 
#                       "[not implemented] northern limit umbra - lon", 
#                       "[not implemented] northern limit umbra - lat", 
#                       "[not implemented] southern limit umbra - lon", 
#                       "[not implemented] southern limit umbra - lat", 
#                       "[not implemented] northern limit penumbra - lon", 
#                       "[not implemented] northern limit penumbra - lat", 
#                       "[not implemented] southern limit penumbra - lon", 
#                       "[not implemented] southern limit penumbra - lat",
#                       rep(NA, each = 5))
# 
# ec_where_attr.attr <- c("fraction of solar diameter covered by moon", 
#                         "ratio of lunar diameter to solar one", 
#                         "fraction of solar disc covered by moon (obscuration)", 
#                         "diameter of core shadow in km", 
#                         "azimuth of sun at tjd", 
#                         "true altitude of sun above horizon at tjd", 
#                         "apparent altitude of sun above horizon at tjd", 
#                         "angular distance of moon from in degrees", 
#                         "eclipse magnitude", 
#                         "saros series number", 
#                         "saros series member number", 
#                         rep(NA, each = 9))

usa.states <- tigris::states(T)
yes.states <- state.abb[!state.abb %in% c("AK", "HI")]

usa.bbox <- sf::st_bbox(usa.states[usa.states$STUSPS %in% yes.states,])


grid.jtimes[between(grid.jtimes$lon, 
                    usa.bbox["xmin"], 
                    usa.bbox["xmax"]) & 
              between(grid.jtimes$lat, 
                      usa.bbox["ymin"], 
                      usa.bbox["ymax"]),]

ggplot() + 
  geom_sf(data = usa.states[usa.states$STUSPS %in% yes.states,]) +
  geom_point(data = grid.jtimes[between(grid.jtimes$lon, usa.bbox["xmin"], usa.bbox["xmax"]) & between(grid.jtimes$lat, usa.bbox["ymin"], usa.bbox["ymax"]),],
             aes(x = lon, y = lat, 
                 size = abs(shadow_dia_km), 
                 color = obs_pct))+
  #scale_size_area()
  scale_color_viridis_c(option = "C")

