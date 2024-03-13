library(renv)
library(swephR)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tigris)
library(readr)

renv::snapshot()
renv::status()
#renv::restore()


rm(list=ls());cat('\f')
getwd()

# Funs----
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

# Vars----
jdate.start <- swephR::swe_utc_to_jd(2013,08,16, #2024,04,5,
                                     4,30,0,1)$dret[1]
by.lon.inc <- 0.5
by.lat.inc <- by.lon.inc


# maps----
usa.states <- tigris::states(T)
yes.states <- state.abb[!state.abb %in% c("AK", "HI")]

usa.bbox <- sf::st_bbox(usa.states[usa.states$STUSPS %in% yes.states,])

grid.usa <- expand.grid(lon = seq(ceiling(usa.bbox["xmin"]), 
                                  floor(usa.bbox["xmax"]), 
                                  by = by.lon.inc), 
                        lat = seq(ceiling(usa.bbox["ymin"]), 
                                  floor(usa.bbox["ymax"]), 
                                  by = by.lat.inc)) %>%
  as_tibble() #%>%
  #mutate(., 
  #       next_t_ecl = NA)

grid.usa2 <- read_csv("~/R/play/eclipse/data/gridusa2.csv")
grid.usa2$next_t_ecl <- as.numeric(grid.usa2$next_t_ecl)
grid.usa2$next_t_ecl <- ifelse(is.na(grid.usa2$next_t_ecl), 
                               -999999, grid.usa2$next_t_ecl) 

grid.usa <- full_join(grid.usa, grid.usa2)
grid.usa$next_t_ecl <- as.numeric(grid.usa$next_t_ecl)


usa.states <- usa.states[usa.states$STUSPS %in% yes.states,]

#grid.jtimes[between(grid.jtimes$lon, usa.bbox["xmin"], usa.bbox["xmax"]) & between(grid.jtimes$lat, usa.bbox["ymin"], usa.bbox["ymax"]),]



(plot.usa <- ggplot()+
  geom_sf(data = usa.states[usa.states$STUSPS %in% yes.states,], 
          color = "white", fill = "grey") )+
  geom_point(data = grid.usa2[grid.usa2$next_t_ecl != -999999,] , 
             aes(x = lon, y = lat))


# hudson, ohio coords----
# lonlat.start <- c(x = -81.44067000, 
#                   y = 41.24006000)

# loop----
# last.finished.row <- max(which(!is.na(grid.usa$next_t_ecl)))
# last.finished.row <- ifelse(is.infinite(last.finished.row),0,last.finished.row)
# last.finished.row <- ifelse(is.na(last.finished.row),0,last.finished.row)

for(i in 4291:nrow(grid.usa)){
  if((i %% 100) == 0){
    #print(paste(i,  nrow(grid.usa), sep = "/", collapse = "/"))
    print(scales::percent(i/nrow(grid.usa)))
  }
  
  # check to see if row's been filled yet
  if(is.na(grid.usa2$next_t_ecl[i]) ){
    # check to see if inside one of states
    
    in.any.state <- F
    i_state <- "AA"
    while(in.any.state == F & !is.na(i_state)){
      for(i_state in c(sample(unique(usa.states$STUSPS)), NA)){
        if(is.na(i_state)){
          break
        }
        temp.state.bbox <- sf::st_bbox(usa.states[usa.states$STUSPS == i_state,])
        
        # check
        if(grid.usa$lon[i] >= temp.state.bbox["xmin"] & 
           grid.usa$lon[i] <= temp.state.bbox["xmax"] &
           grid.usa$lat[i] >= temp.state.bbox["ymin"] & 
           grid.usa$lat[i] <= temp.state.bbox["ymax"]) {
          in.any.state <- T
          break
        }
        
      }
    }
    
    if(in.any.state){
      grid.usa$next_t_ecl[i] <- get_next_total_eclipse(jdate = jdate.start, 
                                                       lonlat = c("x" = grid.usa$lon[i], 
                                                                  "y" = grid.usa$lat[i])) %>% as_date()
    }
  }
  
}

grid.usa$next_t_ecl <- grid.usa$next_t_ecl %>% as_date()

plot.usa + 
  geom_point(data = grid.usa[complete.cases(grid.usa),], 
             aes(x = lon, y = lat, 
             color = year(next_t_ecl) - (year(Sys.Date())+1))) +
  scale_color_viridis_c(option = "C", trans = "log10")+
  theme(legend.position = "bottom", 
        legend.direction = "vertical")
  
plot.usa + 
  geom_contour(data = grid.usa[complete.cases(grid.usa),], 
             aes(x = lon, y = lat, 
                 z = (1/(year(next_t_ecl) - year(Sys.Date())+1))))

# v <- ggplot(faithfuld, aes(waiting, eruptions, z = density))
# v + 
#   geom_contour() +
#   geom_point(data = faithfuld, 
#              aes(x = waiting, y = eruptions, color = density))


 # write_csv(x = grid.usa,
 #            file = "~/R/play/eclipse/data/gridusa2.csv")
