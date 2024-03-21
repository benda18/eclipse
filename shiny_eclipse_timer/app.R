#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# live link: https://tim-bender.shinyapps.io/shiny_eclipse_planner/


#library(renv)
library(swephR)
library(lubridate)
#library(dplyr)
#library(tigris)
library(shiny)
library(censusxy)
library(scales)
library(ggplot2)
library(sf)
library(glue)
#library(rsconnect)

# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("April 8th, 2024 Eclipse Planning Tool -
             Find out if and when a specific location will see totality."),
  sidebarLayout(
    sidebarPanel(
      shiny::textInput(inputId = "addr_in", 
                       label = "Enter Address", 
                       value = "6880 Springfield Xenia Rd, Yellow Springs, OH"),
      actionButton(inputId = "cxy_go", 
                   label   = "SEARCH ADDRESS"), 
      wellPanel(
        fluidRow(" "),
        fluidRow("ACKNOWLEDGEMENTS"),
        wellPanel(
          fluidRow("The geocoding utility relies on a library develped by and described in a 2021 paper in \"Transactions in GIS\" by Prener and Fox, and uses the US Census Bureau's Geocoder API")
        ),
        fluidRow("LINKS"),
        wellPanel(
          fluidRow(uiOutput("tab.res")),
          fluidRow(uiOutput("tab.api")),
          fluidRow(uiOutput("tab.cxy")),
          fluidRow(uiOutput("tab.src"))
        ),
        wellPanel(
          fluidRow(
            uiOutput("tab")
          )
        ),
        fluidRow(HTML('<iframe width="100%" height="auto" aspect-ratio: 16-9 src="https://www.youtube.com/embed/791qJZivHpk?si=1dezKelYKTVQXEkf" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>'))
      )
    ),
    mainPanel(
      wellPanel(
        fluidRow("SEARCH RESULTS:"),
        #fluidRow(shiny::tableOutput(outputId = "return_eclips.times")), 
        fluidRow(shiny::textOutput(outputId = "return_matched.addr")), # returned address
        fluidRow(textOutput(outputId = "return_suncov")), # max sun coverage
        fluidRow(textOutput(outputId = "return_totality")), #totality? goes here
        #fluidRow("Totality Duration: ") # duration of Totality goes here
      ),
      # # new panel for timeline plot----
      # wellPanel(
      #   shiny::plotOutput(outputId = "timeline"),
      # ),
      wellPanel(
        shiny::plotOutput(outputId = "sched"),
        shiny::plotOutput(outputId = "map")
        
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # funs----
  ec_sched <- function(lon_in, lat_in, time_ny){
    # time logic
    
    if(!is.POSIXct(time_ny)){
      stop("'time_ny' input must be of class 'POSIXct' or 'POSIXt'.
         Use lubridate::ymd_hms() or similar, and ensure timezone 
         set to America/NewYork.")
    }
    
    greg_dt.local <- time_ny
    tz.local      <- tz(greg_dt.local)
    
    # do the time conversions
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
    
    # get start, max, end
    ecl_times <- data.frame(st = with_tz(ymd_hms(paste(swe_jdet_to_utc(jd_et = ewl_out$tret[2], 
                                                                       gregflag = 1),
                                                       collapse = "-")), 
                                         tzone = "America/New_York"), 
                            mt = with_tz(ymd_hms(paste(swe_jdet_to_utc(jd_et = ewl_out$tret[1], 
                                                                       gregflag = 1),
                                                       collapse = "-")), 
                                         tzone = "America/New_York"), 
                            et = with_tz(ymd_hms(paste(swe_jdet_to_utc(jd_et = ewl_out$tret[5], 
                                                                       gregflag = 1),
                                                       collapse = "-")), 
                                         tzone = "America/New_York"))
    
    ecl_times2 <- as.vector(ecl_times)
    
    break_mins <- 10
    end_time <- ecl_times2$st 
    n        <- 0
    
    out.times <- unlist(unname(ecl_times2))
    
    while(end_time < ecl_times2$et){
      n <- n + 1
      if(n > 1000){
        stop("error - N")
      }
      
      
      end_time <- end_time %m+% minutes(break_mins)
      
      if(end_time < ecl_times2$et){
        out.times <- c(out.times, 
                       end_time)
      }
    }
    
    out.times <- with_tz(as_datetime(sort(unique(out.times))), tzone = "America/New_York") 
    
    # / start, max, end
    ecsched.times <- out.times
    
    # get sun coverage
    # convert time to julian
    ecsuncov <- NULL
    
    for(i in 1:length(ecsched.times)){
      i_time <-  swephR::swe_julday(year = year(with_tz(ecsched.times[i],"UTC")), 
                                    month = lubridate::month(with_tz(ecsched.times[i],"UTC")), 
                                    day = mday(with_tz(ecsched.times[i],"UTC")), 
                                    hourd = lubridate::hour(with_tz(ecsched.times[i],"UTC")) + 
                                      (lubridate::minute(with_tz(ecsched.times[i],"UTC"))/60), 
                                    gregflag = 1)
      
      ecsuncov <- c(ecsuncov, 
                    swephR::swe_sol_eclipse_how(jd_ut = i_time, 
                                                ephe_flag = 4, 
                                                geopos = c(x = lon_in, 
                                                           y = lat_in, 
                                                           z = 10))$attr[1])
    }
    
    out <- data.frame(time = ecsched.times, 
                      coverage = ecsuncov)
    
    out[nrow(out),]$coverage <- 0
    
    #if(any(out$coverage >= 1)){
      out$coverage[out$coverage >= 1] <- 1.14
    #}
    
    return(out)
  }
  # other stuff---
  usa.states <- readRDS("usastates.rds") 
  
  
  path.files <- list.files(pattern = "^eclpathdfusa.*\\.rds$")
  
  all.paths <- NULL
  for(i in path.files){
    
    try(temp.date <- gsub(pattern = "^eclpathdfusa|\\.rds$", "", i) |> ymd())
    try(temp.paths <- readRDS(i) |> transform(ed = temp.date))
    try(all.paths <- rbind(all.paths, 
                           temp.paths))
    rm(temp.date, temp.paths)
  }
  all.paths <- all.paths |> transform (yr = year(ed))
  
  url <- a("link to interactive map from National Solar Observatory", 
           href="https://nso.edu/for-public/eclipse-map-2024/", 
           target="_blank")
  output$tab <- renderUI({
    tagList("See Also:", url)
  })
  
  url.source <- a("Source Code on GitHub", 
                  href = "https://github.com/benda18/shiny_misc/blob/main/eclipse/shiny_eclipse_timer/app.R", 
                  target = "_blank")
  output$tab.src <- renderUI({
    tagList(url.source)
  })
  
  url.cxy <- a("censusxy Library for R on Github", 
               href = "https://github.com/chris-prener/censusxy", 
               target = "_blank")
  output$tab.cxy <- renderUI({
    tagList(url.cxy)
  })
  
  url.api <- a("US Census Bureau's Geocoder API", 
               href = "https://geocoding.geo.census.gov/geocoder/", 
               target = "_blank")
  output$tab.api <- renderUI({
    tagList(url.api)
  })
  
  url.res <- a("Creating open source composite geocoders: Pitfalls and opportunities (Prener & Fox)", 
               href = "https://onlinelibrary.wiley.com/doi/abs/10.1111/tgis.12741", 
               target = "_blank")
  output$tab.res <- renderUI({
    tagList(url.res)
  })
  
  # get eclipse times----
  
  get_cxyinfo <- eventReactive(input$cxy_go, {
    censusxy::cxy_oneline(address = input$addr_in)
  })
  
  matched_addr <- eventReactive(eventExpr = input$cxy_go, {  # returned address
    temp <- get_cxyinfo()
    matched.addr <- temp$matchedAddress
    matched.addr
  })
  
  # get totality
  get_totality <- eventReactive(eventExpr = input$cxy_go, {
    temp          <- get_cxyinfo()
    lon_in        <- temp$coordinates.x
    lat_in        <- temp$coordinates.y
    greg_dt.local <- ymd_hm("2024-04-07 08:30AM", tz = "America/New_York")
    tz.local      <- tz(greg_dt.local)
    
    # convert to utc
    greg_dt.utc   <- with_tz(greg_dt.local, tz = "UTC")
    jul_dt.utc    <- swephR::swe_julday(year  = year(greg_dt.utc), 
                                        month = lubridate::month(greg_dt.utc, label = F), 
                                        day   = mday(greg_dt.utc), 
                                        hourd = hour(greg_dt.utc) + 
                                          (minute(greg_dt.utc)/60) + 
                                          (second(greg_dt.utc)/60/60), 
                                        gregflag = 1)
    
    # do eclipse math
    sol_cov     <- swephR::swe_sol_eclipse_when_loc(jd_start  = jul_dt.utc, 
                                                    ephe_flag = 4, 
                                                    geopos    = c(x = lon_in,
                                                                  y = lat_in,
                                                                  z = 10), 
                                                    backward = F)$attr[1]
    glue("Totality Visible: {as.character(sol_cov >= 1)}")
  })
  output$return_totality <- renderText({
    get_totality()
  })
  
  
  # get sun coverage
  get_suncov <- eventReactive(eventExpr = input$cxy_go, {
    #"[enter sun coverage calulcations here]"
    
    temp          <- get_cxyinfo()
    lon_in        <- temp$coordinates.x
    lat_in        <- temp$coordinates.y
    greg_dt.local <- ymd_hm("2024-04-07 08:30AM", tz = "America/New_York")
    tz.local      <- tz(greg_dt.local)
    
    # convert to utc
    greg_dt.utc   <- with_tz(greg_dt.local, tz = "UTC")
    jul_dt.utc    <- swephR::swe_julday(year  = year(greg_dt.utc), 
                                        month = lubridate::month(greg_dt.utc, label = F), 
                                        day   = mday(greg_dt.utc), 
                                        hourd = hour(greg_dt.utc) + 
                                          (minute(greg_dt.utc)/60) + 
                                          (second(greg_dt.utc)/60/60), 
                                        gregflag = 1)
    
    # do eclipse math
    sol_cov     <- swephR::swe_sol_eclipse_when_loc(jd_start  = jul_dt.utc, 
                                                    ephe_flag = 4, 
                                                    geopos    = c(x = lon_in,
                                                                  y = lat_in,
                                                                  z = 10), 
                                                    backward = F)$attr[1]
    glue("Maximum Sun Coverage: {ifelse(sol_cov < 1 & sol_cov > 0.99, \"99.0%\", scales::percent(sol_cov,accuracy = 0.1))}")
    
    
  })
  output$return_suncov <- renderText({
    get_suncov()
  })
  
  
  # get_times <- eventReactive(eventExpr = input$cxy_go, {
  #   #temp          <- censusxy::cxy_oneline(address = input$addr_in)
  #   temp          <- get_cxyinfo()
  #   lon_in        <- temp$coordinates.x
  #   lat_in        <- temp$coordinates.y
  #   greg_dt.local <- ymd_hm("2024-04-07 08:30AM", tz = "America/New_York")
  #   tz.local      <- tz(greg_dt.local)
  #   
  #   # convert to utc
  #   greg_dt.utc   <- with_tz(greg_dt.local, tz = "UTC")
  #   jul_dt.utc    <- swephR::swe_julday(year  = year(greg_dt.utc), 
  #                                       month = lubridate::month(greg_dt.utc, label = F), 
  #                                       day   = mday(greg_dt.utc), 
  #                                       hourd = hour(greg_dt.utc) + 
  #                                         (minute(greg_dt.utc)/60) + 
  #                                         (second(greg_dt.utc)/60/60), 
  #                                       gregflag = 1)
  #   
  #   # do eclipse math
  #   ewl_out     <- swephR::swe_sol_eclipse_when_loc(jd_start  = jul_dt.utc, 
  #                                                   ephe_flag = 4, 
  #                                                   geopos    = c(x = lon_in,
  #                                                                 y = lat_in,
  #                                                                 z = 10), 
  #                                                   backward = F)
  #   # extract needed values
  #   ewl_out$tret <- ewl_out$tret[1:5]
  #   ewl_out$attr <- ewl_out$attr[1+c(2)]
  #   
  #   # build output table to be displayed on shiny app
  #   out.times <- data.frame(time_val.jul     = ewl_out$tret, 
  #                           local_time       = NA)
  #   
  #   # convert julian times to gregorian
  #   for(i in 1:nrow(out.times)){
  #     out.times$local_time[i] <- swephR::swe_jdet_to_utc(jd_et = ewl_out$tret[i], 
  #                                                        gregflag = 1) |>
  #       paste(sep = "-", collapse = "-") |>
  #       ymd_hms() |>
  #       with_tz(tzone = tz.local) |>
  #       strftime(format = "%m-%d-%y %I:%M:%S%p %Z", 
  #                tz = tz.local) |>
  #       as.character() 
  #   }
  #   
  #   out.times <- out.times[order(out.times$local_time),]
  #   
  #   # times labels
  #   out.times$eclipse_event <- c("start", 
  #                                "start_totality", 
  #                                "max_eclipse", 
  #                                "end_totality", 
  #                                "end")
  #   rownames(out.times) <- 1:nrow(out.times)
  #   
  #   # filter down to needed columns only
  #   out.times <- out.times[,c("eclipse_event", "local_time")]
  #   
  #   # out_attributes
  #   out.attr <- data.frame(longitude = lon_in, 
  #                          latitude  = lat_in,
  #                          total_ecl_at_loc = ewl_out$attr > 1)
  #   rownames(out.attr) <- 1:nrow(out.attr)
  #   out.attr
  #   
  #   # total vs partial eclipse
  #   if(!out.attr$total_ecl_at_loc){
  #     out.times$local_time[1:5] <- gsub("^.*-\\d{2,2} ", "", out.times$local_time[1:5])
  #     out.times$local_time[1:5] <- gsub("^0", "", out.times$local_time[1:5])
  #     out.times$local_time[1:5] <- gsub("AM ", "am ", out.times$local_time[1:5])
  #     out.times$local_time[1:5] <- gsub("PM ", "pm ", out.times$local_time[1:5])
  #     
  #     # manual fixes because when partial eclipse the order is different
  #     # out.times$local_time[1]  #  no change needed
  #     out.times$local_time[5] <- out.times$local_time[3] 
  #     out.times$local_time[3] <- out.times$local_time[2]
  #     # out.times$local_time[2] <- "<<<partial eclipse only>>>" #  not seen
  #     # out.times$local_time[4] <- "<<<partial eclipse only>>>" #  not seen 
  #     
  #     out.times <- out.times[c(1,3,5),]
  #     out.times$eclipse_type <- c("Partial")
  #   }else{
  #     out.times$local_time <- gsub("^.*-\\d{2,2} ", "", out.times$local_time)
  #     out.times$local_time <- gsub("^0", "", out.times$local_time)
  #     out.times$local_time <- gsub("AM ", "am ", out.times$local_time)
  #     out.times$local_time <- gsub("PM ", "pm ", out.times$local_time)
  #     out.times$eclipse_type <- c("Total")
  #   }
  #   
  #   # adjust sun obscuration > 100% 
  #   out.times$max_sun_obscured <- scales::percent(ifelse(test = ewl_out$attr > 1,
  #                                                        #yes = 1, 
  #                                                        yes = ewl_out$attr,
  #                                                        no  = ewl_out$attr), 
  #                                                 accuracy = 0.1)
  #   
  #   # fix round-up errors
  #   if(ewl_out$attr < 1 & 
  #      grepl("^100", out.times$max_sun_obscured[median(1:nrow(out.times))])){
  #     out.times$max_sun_obscured <- "99.9%"
  #   }
  #   
  #   out.times$max_sun_obscured[c(1:nrow(out.times) != median(1:nrow(out.times)))] <- NA
  #   out.times
  #   
  # })
  # 
  # output$return_eclips.times <- renderTable({
  #   get_times()
  # })
  
  output$return_matched.addr <- renderText({
    matched_addr()  # returned address
  })
  
  output$map <- renderPlot({
    
    addr.coords <- get_cxyinfo()[c("coordinates.x", "coordinates.y")]
    
    
    ggplot() + 
      geom_sf(data = usa.states, 
              fill = "dark grey", color = "white")+
      geom_path(data = all.paths[all.paths$yr <= 2024,], 
                aes(x = lon, y = lat, color = factor(yr)), 
                linewidth = 2)+
      geom_point(aes(x = addr.coords$coordinates.x, 
                     y = addr.coords$coordinates.y),
                 shape = 21,
                 size = 4, color = "white", fill = "red")+
      theme_void()+
      theme(text = element_text(size = 12))+
      coord_sf()+
      scale_color_discrete(name = "Eclipse Path")+
      labs(title = "Eclipse Path")
  })
  
  output$sched <- renderPlot({
    
    addr.coords <- get_cxyinfo()[c("coordinates.x", "coordinates.y")]
    df.sched <- ec_sched(addr.coords$coordinates.x, 
                         addr.coords$coordinates.y, 
                         ymd_hms("2024-04-07 08:30:00", tz = "America/New_York"))
    
    ggplot() + 
      geom_polygon(data = df.sched, 
                 aes(x = time, y = coverage), 
                 alpha = 0.5) +
      # theme_void()+
      scale_y_continuous(name = "Sun Coverage (%)", 
                         labels = scales::percent, 
                         limits = c(0, 1.25), 
                         breaks = seq(0, 2, by = 0.2))+
      scale_x_datetime(name = "Time", 
                       date_labels = "%I:%M %p", 
                       date_breaks = "15 min")+
      theme(title = element_text(size = 12), 
            axis.text.y = element_text(size = 12), 
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12))+
      # scale_color_discrete(name = "Eclipse Path")+
       labs(title = "Percentage of Sun Covered by Moon by Time of Day")
  })
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)