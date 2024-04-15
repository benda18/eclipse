library(renv)
library(dplyr)
library(swephR)
library(leaflet)
library(censusxy)
library(lubridate)
library(ggplot2)

#snapshot()

rm(list=ls());cat('\f');gc()

# Vars----
the.addr <- "100 Main St., Durham NC"
the.datetime.local <- ymd(20240401)
n_points <- 100


# Tidy----
the.datetime.utc <- with_tz(the.datetime.local, "UTC")
lonlat <- unname(censusxy::cxy_oneline(the.addr)[c("coordinates.x", "coordinates.y")])


# find the next eclipse globally (start & end)
df.out <- NULL
# df.ecltype <- data.frame(ecltype.num2 = 1:4, 
#                          ecltype.num = c(4,8,16,32), 
#                          ecltype.name = c("Total", "Annular", 
#                                           "Partial", "Hybrid"))

for(i10 in 1:50){
  if(i10 > 1){
    the.datetime.utc <- max(df.out$gregtime.utc) %m+% days(1)
  }
  
  #for(ecltype in c(1:4)){ # 4 = total, 8 = annular, 16 = partial, 32 = hybrid
    temp <- swe_sol_eclipse_when_glob(jd_start = unique(
      as.integer(
        swe_utc_to_jd(year     = year(the.datetime.utc), 
                      month    = lubridate::month(the.datetime.utc), 
                      day      = mday(the.datetime.utc), 
                      houri    = hour(the.datetime.utc), 
                      min      = minute(the.datetime.utc), 
                      sec      = second(the.datetime.utc), 
                      gregflag = 1)$dret)), 
      ephe_flag = 4, 
      ifltype   = SE$ECL_TOTAL, 
      backward  = F)
    
    next_ecl_glob.times <- temp$tret[c(3,4)]
    
    # if(temp$serr == ""){
    #   break
    # }
    
 # }
  
  #ecl_type <- df.ecltype$ecltype.name[df.ecltype$ecltype.num == ecltype]
  
  
  next_ecl_glob.times <- seq(from = min(next_ecl_glob.times), 
                             to   = max(next_ecl_glob.times), 
                             length.out = n_points)
  
  # find the lon / lat of the eclipse during the eclipse period
  
  
  for(i in next_ecl_glob.times){
    temp <- swe_sol_eclipse_where(jd_ut = i, 
                                  ephe_flag = 4)
    df.out <- rbind(df.out, 
                    data.frame(ecl_id = NA,
                               jul_time.utc = i, 
                               gregtime.utc = ymd_hms(paste(swephR::swe_jdet_to_utc(i, 1), 
                                                            sep = "-", collapse = "-")), 
                               lon      = temp$pathpos[1], 
                               lat      = temp$pathpos[2], 
                               obscuration = temp$attr[3],
                               type = NA,
                               #dia_shad = temp$attr[4], 
                               #azim_sun = temp$attr[5], 
                               alt_sunT = temp$attr[6]))
    #df.out$ecl_id[is.na(df.out$ecl_id)] <- as.character((df.out$gregtime.utc[is.na(df.out$ecl_id)])) 
    
    
  }
}

df.out$ecl_id <- paste("ec", 
                       gsub("-", "", as.character(as_date(df.out$gregtime.utc))), 
                       #df.out$type, 
                       sep = "_")



df.out <- df.out[floor(df.out$alt_sunT) > 0,]

ggplot(data = df.out, 
       aes(x = lon, y = lat, 
           group = ecl_id)) + 
  geom_path()

leaflet() %>%
  addTiles() %>%
  #setView(lonlat[1], lonlat[2], zoom = 1) %>%
  fitBounds(lng1 = min(c(unlist(lonlat[1]), df.out$lon)), 
            lat1 = min(c(unlist(lonlat[2]), df.out$lat)), 
            lng2 = max(c(unlist(lonlat[1]), df.out$lon)), 
            lat2 = max(c(unlist(lonlat[2]), df.out$lat))) %>%
  addMarkers(lng = as.numeric(lonlat[1]), lat = as.numeric(lonlat[2]), 
             label = "You're Here") %>%
  leaflet::addCircles(lng = df.out$lon,
                        lat = df.out$lat,
                        #layerId = df.out$ecl_id, 
                        group = df.out$ecl_id) %>%
  addLayersControl(overlayGroups = df.out$ecl_id)
# leaflet::addCircles(lng = df.out$lon, 
#                     lat = df.out$lat)
