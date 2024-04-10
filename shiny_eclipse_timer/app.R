#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# live link: https://tim-bender.shinyapps.io/shiny_eclipse_planner/


library(renv)
library(jpeg)
library(swephR)
library(lubridate)
library(dplyr)
#library(tigris)
library(shiny)
library(censusxy)
library(scales)
library(ggplot2)
library(sf)
library(glue)
#library(rsconnect)
library(ggmap)
library(png)
library(grid)
library(qrcode)


# renv::status()
# renv::snapshot()

# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("Total Eclipse of April 8, 2024 - What to Expect from Your Location"),
  sidebarLayout(
    sidebarPanel(
      shiny::textInput(inputId = "addr_in", 
                       label = "Enter Street Address [general terms like \'The White House\' won't work]" , 
                       value = sample(x = c("1600 Pennsylvania Ave, Washington, DC",      
                                            "1060 W Addison, Chicago IL",               
                                            "1 Bear Valley Rd, Point Reyes Station, CA",  
                                            "250 E Franklin St, Chapel Hill, NC",          
                                            "100 Joe Nuxhall Wy, Cincinnati, OH",        
                                            "281 W Lane Ave, Columbus, OH",               
                                            "300 Alamo Plaza, San Antonio, TX",           
                                            "2634 Main St, Lake Placid, NY",             
                                            "1047 Main St, Buffalo, NY" ,                 
                                            "2610 University Cir, Cincinnati, OH",     
                                            "3159 W 11th St, Cleveland, OH",              
                                            "4001 W 2nd St, Roswell, NM",              
                                            "926 E McLemore Ave, Memphis, TN",           
                                            "369 Central Ave, Hot Springs, AR",         
                                            "4790 W 16th St, Indianapolis, IN"), 
                                      size = 1)),
      actionButton(inputId = "cxy_go", 
                   label   = "SEARCH ADDRESS"),
      
      wellPanel(
        #shiny::plotOutput("plot_flawless", width = "300px", height = "250px"),
        fluidRow(div(h4(strong(textOutput(outputId = "return_suncov"))))), # max sun coverage
        fluidRow(div(h4(span(textOutput(outputId = "return_tot.dur"), style = "color:red")))),
        fluidRow(div(h4(strong(textOutput(outputId = "return_nextecl"))))),
        #fluidRow(div(h4(strong("Next View of Totality: [temporarily removed due to bug]")))),
        #fluidRow(uiOutput("nextecl_dash"))
      ),
      wellPanel(
        shiny::plotOutput(outputId = "sched"),
      ),
      wellPanel(
        # wellPanel(
        #   fluidRow("Developed by Tim Bender"), 
        #   fluidRow(uiOutput("tab.linkedin")),
        #   fluidRow(uiOutput("tab.github"))
        # ),
        fluidRow(HTML('<iframe width="100%" height="auto" aspect-ratio: 16-9 src="https://www.youtube.com/embed/791qJZivHpk?si=1dezKelYKTVQXEkf" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>')),
        # wellPanel(
        #   fluidRow("RESOURCES"),
        #   fluidRow(uiOutput("tab.res2")),
        #   #fluidRow(uiOutput("tab.nasa")),
        #   fluidRow(uiOutput("tab")),
        #   #fluidRow("SOURCES"),
        #   fluidRow(uiOutput("tab.res"))
        # ),
      )
    ),
    mainPanel(
      # BLOCK RESOURCES MAIN PANEL----
      wellPanel(
        fluidRow(strong("RESOURCES")),
        fluidRow(uiOutput("tab.res2")),
        fluidRow(uiOutput("tab.nasa")),
        fluidRow(uiOutput("tab")),
        fluidRow(uiOutput("tab.res")),
        fluidRow(strong("DEVELOPED BY")), 
        fluidRow(uiOutput("tab.linkedin")),
        fluidRow(strong("SPECIAL THANKs")),
        fluidRow("Special thanks to reddit users u/danielsixfive and u/QuackingUp23"),
        fluidRow(strong("SOURCES")),
        fluidRow(uiOutput("tab.github")),
        fluidRow(strong("DONATIONS - help cover hosting costs")), 
        fluidRow(uiOutput("tab.venmo"))
      ),
      #/BRMP
      wellPanel(
        shiny::plotOutput(outputId = "map"),
        shiny::plotOutput(outputId = "qr_url", 
                          height = "200px")
        ), 
      # wellPanel(
      #   fluidRow(strong("DONATIONS - help cover hosting costs")), 
      #   #fluidRow("This tool was created for fun for the enjoyment and use of others, and was built upon the work of others who came before me. There is montly cost to keep it live for people to use, so if you want to donate to help cover that cost or even a little extra I would appreciate it, but do not expect it"), 
      #   #fluidRow("Venmo: @Tim_J_Bender"), 
      #   fluidRow(uiOutput("tab.venmo"))
      # )
      wellPanel(
        fluidRow(strong("OTHER ECLIPSE WEBAPPS")), 
        fluidRow(uiOutput("tab.AE")), 
        fluidRow(uiOutput("tab.NE"))
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
    end_time   <- ecl_times2$st 
    n          <- 0
    
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
                    max(swephR::swe_sol_eclipse_how(jd_ut = i_time, 
                                                ephe_flag = 4, 
                                                geopos = c(x = lon_in, 
                                                           y = lat_in, 
                                                           z = 10))$attr[c(1,3)]))
    }
    
    out <- data.frame(time = ecsched.times, 
                      coverage = ecsuncov)
    
    out[nrow(out),]$coverage <- 0
    return(out)
  }
  # other stuff---
  usa.states <- readRDS("usastates.rds") 
  path.files <- list.files(pattern = "^eclpathdfusa.*\\.rds$") # this line of code pulls in all paths that are in the home directory. 
  
  all.paths <- NULL
  for(i in path.files){
    
    try(temp.date <- gsub(pattern = "^eclpathdfusa|\\.rds$", "", i) |> ymd())
    try(temp.paths <- readRDS(i) |> transform(ed = temp.date))
    try(all.paths <- rbind(all.paths, 
                           temp.paths))
    rm(temp.date, temp.paths)
  }
  all.paths <- all.paths |> transform(yr = year(ed))
  
  url <- a("National Solar Observatory's 2024 Interactive Eclipse Map", 
           href="https://nso.edu/for-public/eclipse-map-2024/", 
           target="_blank")
  output$tab <- renderUI({
    tagList(url)
  })
  
  # url.nextecl_dash <- a("Want to know more? Click here to find the next solar and lunar eclipse for any location at any point in recent history (today +/- 1000 years)", 
  #                  href="https://tim-bender.shinyapps.io/shiny_next_eclipse/", 
  #                  target="_blank")
  # output$nextecl_dash <- renderUI({
  #   tagList(url.nextecl_dash)
  # })
  # 
  url.nasa <- a("NASA's 2024 Eclipse Website",
                href = "https://science.nasa.gov/eclipses/future-eclipses/eclipse-2024/",
                target = "_blank")
  output$tab.nasa <- renderUI({
    tagList(url.nasa)
  })
  # RESOURCES----
  url.venmo <- a("Venmo: @Tim_J_Bender", 
                 href = "https://venmo.com/u/Tim_J_Bender", 
                 target = "_blank")
  output$tab.venmo <- renderUI({
    tagList(url.venmo)
  })
  
  url.github <- a("Source Code", 
                  href = "https://github.com/benda18/eclipse/blob/main/shiny_all_eclipses/app.R", 
                  target = "_blank")
  output$tab.github <- renderUI({
    tagList(url.github)
  })
  
  url.linkedin <- a("Tim Bender (LinkedIn)", 
                    href = "https://www.linkedin.com/in/tim-bender-238870171/", 
                    target = "_blank")
  output$tab.linkedin <- renderUI({
    tagList(url.linkedin)
  })
  #/RESOURCES
  
  url.res2 <- a("The AAS \"Suppliers of Safe Solar Viewers & Filters\" list", 
                href = "https://eclipse.aas.org/eye-safety/viewers-filters", 
                target = "_blank")
  output$tab.res2 <- renderUI({
    tagList(url.res2)
  })
  
  # get eclipse times----
  
  get_cxyinfo <- eventReactive(input$cxy_go, {
    censusxy::cxy_oneline(address = input$addr_in)
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
    
    # tret 3 & 4 = begin and end of totality
    # do eclipse math
    sol_cov     <- max(swephR::swe_sol_eclipse_when_loc(jd_start  = jul_dt.utc, 
                                                    ephe_flag = 4, 
                                                    geopos    = c(x = lon_in,
                                                                  y = lat_in,
                                                                  z = 10), 
                                                    backward = F)$attr[c(1,3)])
    
    ifelse(sol_cov >= 1, 
           "Within Path of Totality", 
           "Outside Path of Totality")
    
  })
  
  
  #### get next eclipse
  get_nextecl <- eventReactive(eventExpr = input$cxy_go, {
    start.date <- ymd(20240409)
    min_obsc <- 1
    get.addr <- censusxy::cxy_oneline(address = input$addr_in)
    var.lon  <- unlist(unname(get.addr["coordinates.x"])) # runif(1, -180,180)
    var.lat  <- unlist(unname(get.addr["coordinates.y"])) # runif(1, -90, 90)

    is_totality <- F
    n <- 0
    while(!is_totality & year(start.date) < 2501){
      n <- n + 1
      if(n > 2000){
        stop("too many searches - ERROR")
      }
      a.date.ju <- swephR::swe_utc_to_jd(year = year(start.date),
                                         month = lubridate::month(start.date),
                                         day   = mday(start.date),
                                         houri = 0,
                                         min   = 30,
                                         sec   = 0,
                                         gregflag = 1)$dret[2]

      when_next <- swe_sol_eclipse_when_loc(jd_start = a.date.ju,
                                            ephe_flag = 4,
                                            geopos = c(x = var.lon,
                                                       y = var.lat,
                                                       z = 10),
                                            backward = F)

      temp.nextdate <- ymd_hms(paste(swephR::swe_jdet_to_utc(when_next$tret[1], 1),
                                     sep = "-", collapse = "-"))

      temp.nextobs <- max(when_next$attr[c(1,3)])

      if(temp.nextobs >= min_obsc){
        is_totality <- T
        #next.obs <- temp.nextobs
        start.date <- as_date(temp.nextdate)
      }else{
        start.date <- as_date(temp.nextdate) + days(2)
        #next.obs <- temp.nextobs
      }
    }

    # do next----
    if(temp.nextobs < 1 &
       year(start.date) > 2500){
      next.total.eclipse <- "Sometime after the year 2500"
    }else{
      next.total.eclipse <-  strftime(start.date, format = "%B %d, %Y")
    }

    glue("Next View of Totality: {next.total.eclipse}")
  })
  ### /get next eclipse

  
  
  output$return_nextecl <- renderText({
    get_nextecl()
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
    sol_cov     <- max(swephR::swe_sol_eclipse_when_loc(jd_start  = jul_dt.utc, 
                                                    ephe_flag = 4, 
                                                    geopos    = c(x = lon_in,
                                                                  y = lat_in,
                                                                  z = 10), 
                                                    backward = F)$attr[c(1,3)])
    sol_cov <- ifelse(sol_cov > 1, 1, sol_cov)
    glue("{ifelse(sol_cov < 1 & sol_cov >= 0.99, \"99%\", scales::percent(sol_cov,accuracy = 0.1))}")
  })
  
  get_tot.dur <- eventReactive(eventExpr = input$cxy_go, {
    #if(as.numeric(gsub("%", "", x = get_suncov())) >= 100){
    #out <- "yes"
    start.datetime <- ymd_hms("2024-04-07 08:30:00", tz = "America/New_York")
    get.addr <- get_cxyinfo()
    var.lon <- unlist(unname(get.addr["coordinates.x"]))
    var.lat <- unlist(unname(get.addr["coordinates.y"]))
    
    greg_dt.local <- start.datetime
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
    
    out.dur <- swephR::swe_sol_eclipse_when_loc(jd_start = jul_dt.utc, 
                                                ephe_flag = 4, 
                                                geopos = c(x = var.lon, 
                                                           y = var.lat, 
                                                           z = 10), 
                                                backward = F)
    
    difftimes.jul <- out.dur$tret[c(3,4)]
    tot_start <- with_tz(ymd_hms(paste(swephR::swe_jdet_to_utc(difftimes.jul[1], 
                                                               gregflag = 1), 
                                       sep = "-", collapse = "-")), 
                         tz = "America/New_York")
    tot_end   <- with_tz(ymd_hms(paste(swephR::swe_jdet_to_utc(difftimes.jul[2], 
                                                               gregflag = 1), 
                                       sep = "-", collapse = "-")), 
                         tz = "America/New_York")
    out.time.dec <- tot_end - tot_start
    
    if(attributes(out.time.dec)$units == "mins"){
      out.time.dec <- as.numeric(out.time.dec)
      out.time <- c("mins" = floor(out.time.dec), 
                    "secs" = floor((out.time.dec - floor(out.time.dec))*60))
    }else{
      out.time <- c("mins" = "00", 
                    "secs" = as.character(abs(floor(as.numeric(out.time.dec)))))
    }
    
    if(nchar(out.time["secs"])==1){
      out.time["secs"] <- paste("0",out.time["secs"],sep="",collapse="")
    }
    
    
    #out.time <- ifelse(out.time == "00:0", "00:00", out.time)
    
    out <- paste(out.time, sep = ":", collapse = ":")
    ifelse(out %in% c("00:0", 
                      "00:00"), 
           "",
           paste("Length of Totality (mm:ss): ", out, sep = "", collapse = ""))
    
    })
  
  output$return_tot.dur <- renderText({
    get_tot.dur()
  })
  
  
  output$return_suncov <- renderText({
    paste(get_suncov(), get_totality(), sep = " | ", collapse = " | ")
    
  })
  
  
  output$qr_url <- renderPlot({
    qr_app <- qrcode::qr_code(x = "https://tim-bender.shinyapps.io/shiny_eclipse_planner/", 
                              ecl = "H")
    qr_app_logo <- add_logo(qr_app, 
                            logo = "www/QRLOGO.jpg")
    plot(qr_app_logo)
  })
  
  output$map <- renderPlot({
    addr.coords <- get_cxyinfo()[c("coordinates.x", "coordinates.y", "matchedAddress")]
    
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
      theme(text = element_text(size = 12), 
            legend.position = "bottom", 
            legend.text = element_text(size = 12))+
      scale_color_discrete(name = NULL)+
      labs(title = "Eclipse Path")
  })
  
  output$sched <- renderPlot({
    
    addr.coords <- get_cxyinfo()[c("coordinates.x", "coordinates.y", "matchedAddress")]
    
    # undesirable behavior happening here - if censusxy does not find an
    # appropriate address, a null value gets input into the function below
    
    df.sched <- ec_sched(unname(unlist(addr.coords$coordinates.x)), 
                         unname(unlist(addr.coords$coordinates.y)), 
                         ymd_hms("2024-04-07 08:30:00", tz = "America/New_York"))
    if(max(df.sched$coverage) >= 1){
      # img <- readPNG(system.file(#"img", "flawless.png", package="png"))
      img <- readPNG("www/totality.png")
      g <- rasterGrob(img, interpolate=TRUE)
      
      sched.plot <- ggplot() + 
        # geom_hline(aes(yintercept = 1, 
        #                color = "Totality"), 
        #            linetype = 2, linewidth = 1)+
        geom_polygon(data = df.sched, alpha = 0.4,
                     aes(x = time, y = coverage), 
                     linewidth = 1) +
        geom_line(data = df.sched, 
                  aes(x = time, y = coverage), 
                  linewidth = 1) +
        scale_y_continuous(name = "% of Sun Obscured by Moon", 
                           labels = scales::percent, 
                           limits = c(0, 1.25), 
                           breaks = seq(0, 1, by = 0.2))+
        scale_x_datetime(name = "Time (Eastern Daylight Time)", 
                         date_labels = "%I:%M %p %Z", 
                         date_breaks = "30 min", 
                         date_minor_breaks = "15 min")+
        scale_color_discrete(name = NULL)+
        theme(legend.position = "bottom",
              legend.text = element_text(size = 12),
              title = element_text(size = 12), 
              axis.text.y = element_text(size = 12), 
              axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12))+
        labs(title = "Eclipse Timeline (Eastern Timezone)", 
             subtitle = unname(unlist(addr.coords$matchedAddress))) +
        geom_vline(aes(xintercept = range(df.sched[df.sched$coverage >= 1,]$time), 
                       color = "Time of Totality")) +
        annotation_custom(g, xmin=-Inf, xmax=Inf, 
                          ymin= 1, 
                          ymax= 1.25)
    }else{
      sched.plot <- ggplot() + 
        geom_hline(aes(yintercept = 1, 
                       color = "Totality"), 
                   linetype = 2, linewidth = 1)+
        geom_polygon(data = df.sched, alpha = 0.4,
                     aes(x = time, y = coverage), 
                     linewidth = 1) +
        geom_line(data = df.sched, 
                  aes(x = time, y = coverage), 
                  linewidth = 1) +
        scale_y_continuous(name = "% of Sun Obscured by Moon", 
                           labels = scales::percent, 
                           limits = c(0, NA), 
                           breaks = seq(0, 2, by = 0.2))+
        scale_x_datetime(name = "Time (Eastern Daylight Time)", 
                         date_labels = "%I:%M %p %Z", 
                         date_breaks = "30 min", 
                         date_minor_breaks = "15 min")+
        scale_color_discrete(name = NULL)+
        theme(legend.position = "bottom",
              legend.text = element_text(size = 12),
              title = element_text(size = 12), 
              axis.text.y = element_text(size = 12), 
              axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12))+
        labs(title = "Eclipse Timeline", 
             subtitle = unname(unlist(addr.coords$matchedAddress)))+
               geom_vline(aes(xintercept = max(df.sched[df.sched$coverage ==
                                                            max(df.sched$coverage),]$time), 
                              color = "Time of Peak Eclipse")) 
    }
    
    
    print(sched.plot + 
            geom_vline(aes(xintercept = Sys.time())))
    
    
  })
  url.AE <- a("* Every Solar Eclipse Visible from Your Address for 75 Years",
              href = "https://tim-bender.shinyapps.io/shiny_all_eclipses/",
              target = "_blank")
  output$tab.AE <- renderUI({
    tagList(url.AE)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)