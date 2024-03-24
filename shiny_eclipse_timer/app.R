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
library(ggmap)

# renv::status()
# renv::snapshot()

# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("Total Eclipse of April 8, 2024 - What to Expect from Your Address"),
  sidebarLayout(
    sidebarPanel(
      shiny::textInput(inputId = "addr_in", 
                       label = "Enter Address", 
                       value = "6880 Springfield Xenia Rd, Yellow Springs, OH"),
      actionButton(inputId = "cxy_go", 
                   label   = "SEARCH ADDRESS"), 
      wellPanel(
        shiny::plotOutput(outputId = "sched"),
      ),
      wellPanel(
        wellPanel(
          fluidRow("Developed by Tim Bender"), 
          fluidRow(uiOutput("tab.linkedin")),
          fluidRow(uiOutput("tab.github"))
        ),
        fluidRow(HTML('<iframe width="100%" height="auto" aspect-ratio: 16-9 src="https://www.youtube.com/embed/791qJZivHpk?si=1dezKelYKTVQXEkf" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>')),
        
        wellPanel(
          fluidRow("RESOURCES"),
          fluidRow(uiOutput("tab.res2")),
          fluidRow("SOURCES"),
          fluidRow(uiOutput("tab.res"))
        ),
      )
    ),
    mainPanel(
      wellPanel(
        fluidRow(div(h5(strong("WHAT TO EXPECT:")))),
        fluidRow(div(h6(strong(shiny::textOutput(outputId = "return_matched.addr"))))), # returned address
        fluidRow(div(h6(strong(textOutput(outputId = "return_suncov"))))), # max sun coverage
        fluidRow(div(h6(strong(textOutput(outputId = "return_totality"))))), #totality? goes here
        fluidRow(div(h6(strong(textOutput(outputId = "return_nextecl")))))
      ),
      wellPanel(
        shiny::plotOutput(outputId = "map"),
        wellPanel(
          fluidRow("Other Useful Maps:",
            uiOutput("tab"),
            uiOutput("tab.nasa")
          )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # api keys----
  #stadiamap set api
  
  apikey <- "5b522e9b-4ea1-4168-b0b9-3294c85af004" # GET YOUR OWN KEY!
  # NOTE - it is not best practices to keep the api key visible here in your
  # public repo.  Ideally you would save it in a separate file and add that file
  # to your .gitignore so it doesn't get pushed to github.  that way the key
  # still gets deployed to the server but remains relatively hidden otherwise.
  # THAT SAID for the sake of simplicity, and because stadiamaps is a free
  # service (for this kind of limited usage) that anyone can setup, and because
  # the risk of abuse affecting anyonse personally is so low I chose to keep it
  # in the code so that it was clear to someone trying to replicate this project
  # how and where to dump their own API key. Additionally, api keys can be
  # generated fairly easily for projects like this and if at some point in the
  # future I change my mind I can implement a more secure setup.
  
  register_stadiamaps(key = apikey, write = FALSE)
  
  
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
                                                           z = 10))$attr[3])
    }
    
    out <- data.frame(time = ecsched.times, 
                      coverage = ecsuncov)
    
    out[nrow(out),]$coverage <- 0
    #out$coverage[out$coverage >= 1] <- 1.14
    return(out)
  }
  # other stuff---
  usa.states <- readRDS("usastates.rds") 
  path.files <- list.files(pattern = "^eclpathdfusa.*\\.rds$") # this line of code pulls in all paths. 
  
  all.paths <- NULL
  for(i in path.files){
    
    try(temp.date <- gsub(pattern = "^eclpathdfusa|\\.rds$", "", i) |> ymd())
    try(temp.paths <- readRDS(i) |> transform(ed = temp.date))
    try(all.paths <- rbind(all.paths, 
                           temp.paths))
    rm(temp.date, temp.paths)
  }
  all.paths <- all.paths |> transform(yr = year(ed))
  
  url.nasa <- a("NASA's 2024 Solar Eclipse Website", 
                href = "https://science.nasa.gov/eclipses/future-eclipses/eclipse-2024/", 
                target = "_blank")
  output$tab.nasa <- renderUI({
    tagList(url.nasa)
  })
  
  url <- a("Interactive map from National Solar Observatory", 
           href="https://nso.edu/for-public/eclipse-map-2024/", 
           target="_blank")
  output$tab <- renderUI({
    tagList(url)
  })
  
  url.github <- a("GitHub", 
                  href = "https://github.com/benda18/eclipse/", 
                  target = "_blank")
  output$tab.github <- renderUI({
    tagList(url.github)
  })
  
  url.linkedin <- a("LinkedIn", 
                    href = "www.linkedin.com/in/tim-bender-238870171", 
                    target = "_blank")
  output$tab.linkedin <- renderUI({
    tagList(url.linkedin)
  })
  
  url.res <- a("All Sources are cited on the project's GitHub README", 
               href = "https://github.com/benda18/eclipse/blob/main/README.md#sources", 
               target = "_blank")
  output$tab.res <- renderUI({
    tagList(url.res)
  })
  
  url.res2 <- a("The American Astronomical Society's \"Suppliers of Safe Solar Viewers & Filters\" list", 
                href = "https://eclipse.aas.org/eye-safety/viewers-filters", 
                target = "_blank")
  output$tab.res2 <- renderUI({
    tagList(url.res2)
  })
  
  # get eclipse times----
  
  get_cxyinfo <- eventReactive(input$cxy_go, {
    censusxy::cxy_oneline(address = input$addr_in)
  })
  
  matched_addr <- eventReactive(eventExpr = input$cxy_go, {  # returned address
    temp <- get_cxyinfo()
    if(!is.null(temp)){
      matched.addr <- temp$matchedAddress
    }else{
      matched.addr <- "<<< NO ADDRESS MATCH FOUND - TRY AGAIN >>>"
    }
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
                                                    backward = F)$attr[3]
    
    
    ifelse(sol_cov >= 1, 
           "Within Zone of Totality", 
           "Outside Zone of Totality")
    
  })
  output$return_totality <- renderText({
    get_totality()
  })
  
  get_nextecl <- eventReactive(eventExpr = input$cxy_go, {
    start.date <- ymd(20240409)
    get.addr <- get_cxyinfo()
    var.lon <- unlist(unname(get.addr["coordinates.x"]))
    var.lat <- unlist(unname(get.addr["coordinates.y"]))
    
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
    
    temp.nextdate <- as_date(ymd_hms(paste(swephR::swe_jdet_to_utc(when_next$tret[1], 1), sep = "-", collapse = "-")))
    temp.nextdate <- strftime(x = temp.nextdate, format = "%b %d, %Y")
    
    temp.nextobs <- scales::percent(when_next$attr[3]) 
    
    glue("Next Eclipse Visible Here: {temp.nextdate} ({temp.nextobs} obscuration)")
    })
  
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
    sol_cov     <- swephR::swe_sol_eclipse_when_loc(jd_start  = jul_dt.utc, 
                                                    ephe_flag = 4, 
                                                    geopos    = c(x = lon_in,
                                                                  y = lat_in,
                                                                  z = 10), 
                                                    backward = F)$attr[3]
    
    sol_cov <- ifelse(sol_cov >= 1, 1, sol_cov)
    glue("Maximum Sun Coverage: {ifelse(sol_cov < 1 & sol_cov > 0.995, \"99.5%\", scales::percent(sol_cov,accuracy = 0.1))}")
  })
  
  output$return_suncov <- renderText({
    get_suncov()
  })
  
  output$return_matched.addr <- renderText({
    matched_addr()  # returned address
  })
  
  output$map <- renderPlot({
    addr.coords <- get_cxyinfo()[c("coordinates.x", "coordinates.y", "matchedAddress")]
    
    usa48.bbox <- c(left   = -124.76307, 
                    bottom =   24.52310, 
                    right  = - 66.94989, 
                    top    =   49.38436)
    bm.stadia <- get_stadiamap(bbox = usa48.bbox, 
                               zoom = 4, 
                               maptype = "stamen_terrain",#"stamen_toner_lite",                            
                               crop = T, 
                               color = "color",
                               force = T,
                               size = 1.04)
    ggmap(bm.stadia)+
    #ggplot() + 
      # geom_sf(data = usa.states, 
      #         fill = "dark grey", color = "white")+
      geom_path(data = all.paths[all.paths$yr <= 2024,], 
                aes(x = lon, y = lat, color = factor(yr)), 
                linewidth = 2)+
      geom_point(aes(x = addr.coords$coordinates.x, 
                     y = addr.coords$coordinates.y),
                 shape = 21,
                 size = 4, color = "white", fill = "red")+
      theme_void()+
      theme(text = element_text(size = 12))+
      #coord_sf()+
      scale_color_discrete(name = "Eclipse Path")+
      labs(title = "Eclipse Path")
  })
  
  output$sched <- renderPlot({
    
    addr.coords <- get_cxyinfo()[c("coordinates.x", "coordinates.y", "matchedAddress")]
    
    # undesirable behavior happening here - if censusxy does not find an
    # appropriate address, a null value gets input into the function below
    
    df.sched <- ec_sched(addr.coords$coordinates.x, 
                         addr.coords$coordinates.y, 
                         ymd_hms("2024-04-07 08:30:00", tz = "America/New_York"))
    
    ggplot() + 
      geom_polygon(data = df.sched, 
                 aes(x = time, y = coverage), 
                 alpha = 0.5) +
      scale_y_continuous(name = "% of Sun Obscured by Moon", 
                         labels = scales::percent, 
                         limits = c(0, 1), 
                         breaks = seq(0, 2, by = 0.2))+
      scale_x_datetime(name = "Time (Eastern Daylight Time)", 
                       date_labels = "%I:%M %p %Z", 
                       date_breaks = "30 min", 
                       date_minor_breaks = "15 min")+
      theme(title = element_text(size = 12), 
            axis.text.y = element_text(size = 12), 
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12))+
       labs(title = "Eclipse Progress Throughout the Day", 
            subtitle = addr.coords$matchedAddress)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)