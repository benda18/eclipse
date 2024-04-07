#
# This is a Shiny web application.
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(swephR)
library(lubridate)
library(dplyr)
#library(tigris)
library(censusxy)
library(scales)
library(ggplot2)
#library(sf)
library(renv)
library(glue)
#library(rsconnect)
library(qrcode)

ui <- fluidPage(
  
  # Application title
  titlePanel("Solar Eclipses Visible from Your Address", ),
  
  sidebarLayout(
    sidebarPanel(
      shiny::textInput(inputId = "addr_in", 
                       label = "Enter Address", 
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
      shiny::dateInput(inputId = "date_in", 
                       label = "Search-From Date", 
                       value = Sys.Date(), 
                       min   = "1000-01-01", 
                       max   = "2999-12-31", 
                       format = "MM dd, yyyy"),
      actionButton(inputId = "search_go", 
                   label   = "SEARCH ADDRESS"),
     
    ),
    
    mainPanel(
      # BLOCK RESOURCES MAIN PANEL----
      wellPanel(
        fluidRow(strong("DEVELOPED BY")), 
        fluidRow(uiOutput("tab.linkedin")),
        fluidRow(strong("SPECIAL ASSISTANCE FROM")),
        fluidRow("reddit user /u/danielsixfive for QA assistance"),
        fluidRow(strong("SOURCES")),
        fluidRow(uiOutput("tab.github")),
        fluidRow(strong("DONATIONS - help cover hosting costs")), 
        fluidRow(uiOutput("tab.venmo")),
      ),
      #/BRMP
      
      wellPanel(
        fluidRow("The table below shows the next 75 years of solar eclipses visible from this location."),
        #fluidRow(strong(span("NOTE: Obscuration percentages were being incorrectly calculated in the table below by about 5% previously. This error has now been fixed.", style = "color:black"))),
      ),
      shiny::tableOutput(outputId = "logtable"),
      shiny::plotOutput(outputId = "qr_url", 
                        height = "200px")
      # wellPanel(
      #   fluidRow(strong("DONATIONS - help cover hosting costs")), 
      #   fluidRow(uiOutput("tab.venmo")),
      #   #fluidRow(uiOutput("addr_img"))
      # )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  eclipsewise_url <- function(ecl_date,# = ymd(20780511), 
                              ecltype = c("Total Eclipse", "Annular", "Partial", "Hybrid")){
    require(glue)
    require(lubridate)
    w.year  <- year(ecl_date)
    w.month <- as.character(lubridate::month(ecl_date, label = F))
    w.month <- ifelse(nchar(w.month) == 1,
                      paste("0", w.month, sep = "", collapse = ""),
                      w.month)
    w.mday  <- mday(ecl_date)
    w.mday  <- ifelse(nchar(w.mday) == 1,
                      paste("0", w.mday, sep = "", collapse = ""),
                      w.mday)
    w.cenA  <- floor(w.year/100)*100+1
    w.cenB  <- w.cenA + 99
    
    et <- toupper(substr(ecltype,1,1))
    
    return(glue("https://eclipsewise.com/solar/SEping/{w.cenA}-{w.cenB}/SE{w.year}-{w.month}-{w.mday}{et}.gif"))
  }
  
  get_search.addr <- eventReactive(input$search_go, {
    censusxy::cxy_oneline(address = input$addr_in)
  })
  
  get_search.date <- eventReactive(input$search_go, {
    input$date_in
  })
  
  output$logtable <- shiny::renderTable({
    # vars----
    start.date      <- get_search.date()
    max.year        <- year(start.date) + 75
    #min_obsc        <- 1 
    
    # do work----
    get.addr <- get_search.addr()
    var.lon <- unlist(unname(get.addr["coordinates.x"])) # runif(1, -180,180) 
    var.lat <- unlist(unname(get.addr["coordinates.y"])) # runif(1, -90, 90)  
    
    ####
    is_totality <- F
    n <- 0
    
    log.ecls <- NULL
    
    while(#!is_totality & 
          year(start.date) < max.year){
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
      # search eclipse type
      ecl_total <- swe_sol_eclipse_when_glob(jd_start = when_next$tret[2]-1, 
                                            ifltype = SE$ECL_TOTAL,#SE$ECL_CENTRAL|SE$ECL_NONCENTRAL,
                                            ephe_flag = 4, 
                                            backward = F)
      ecl_annular <- swe_sol_eclipse_when_glob(jd_start = when_next$tret[2]-1, 
                                               ifltype = SE$ECL_ANNULAR,
                                               ephe_flag = 4, 
                                               backward = F)
      ecl_partial <- swe_sol_eclipse_when_glob(jd_start = when_next$tret[2]-1, 
                                               ifltype = SE$ECL_PARTIAL,
                                               ephe_flag = 4, 
                                               backward = F)
      ecl_hybrid <- swe_sol_eclipse_when_glob(jd_start = when_next$tret[2]-1, 
                                               ifltype = SE$ECL_ANNULAR_TOTAL,
                                               ephe_flag = 4, 
                                               backward = F)
      
      ecl_type222 <- c("Total Eclipse", "Annular", 
                       "Partial", "Hybrid")[which(abs(c(ecl_total$tret[2] - when_next$tret[2],
                                                    ecl_annular$tret[2] - when_next$tret[2],
                                                    ecl_partial$tret[2] - when_next$tret[2],
                                                    ecl_hybrid$tret[2] - when_next$tret[2])) == 
                                                    min(abs(c(ecl_total$tret[2] - when_next$tret[2],
                                                          ecl_annular$tret[2] - when_next$tret[2],
                                                          ecl_partial$tret[2] - when_next$tret[2],
                                                          ecl_hybrid$tret[2] - when_next$tret[2]))))]
      ecl_typecheck0 <- min(abs(c(ecl_total$tret[2] - when_next$tret[2],
                              ecl_annular$tret[2] - when_next$tret[2],
                              ecl_partial$tret[2] - when_next$tret[2],
                              ecl_hybrid$tret[2] - when_next$tret[2])))
      
      
      
      # NEXT DATE
      temp.nextdate <- ymd_hms(paste(swephR::swe_jdet_to_utc(when_next$tret[1], 1), 
                                     sep = "-", collapse = "-"))
      
      temp.nextobs <- max(when_next$attr[c(1,3)]) # p
      
      log.ecls <- rbind(log.ecls,
                        data.frame(#n        = n,
                          #address  = the.addr,
                          #date     = temp.nextdate,
                          Date = strftime(x = temp.nextdate, 
                                          format = "%b %d, %Y", 
                                          tz = "America/New_York"),
                          #jdate    = NA,
                          Type = ecl_type222,
                          #ecl_typecheck0 = ecl_typecheck0,
                          pct_obscured = temp.nextobs, 
                          Eclipse_Map = eclipsewise_url(ecl_date = temp.nextdate, 
                                                        ecltype = ecl_type222)))
      
      temp.utc <- temp.nextdate
      temp.jd  <- swe_utc_to_jd(year = year(temp.utc),
                                month = lubridate::month(x = temp.utc, label = F),
                                day = mday(temp.utc),
                                houri = hour(temp.utc),
                                min = minute(temp.utc),
                                sec = second(temp.utc),
                                gregflag = 1)$dret |>
        as.integer() |>
        unique()
      
      if(year(start.date) >= max.year){
        break
        is_totality <- T
        next.obs <- temp.nextobs
        start.date <- as_date(temp.nextdate)
      }else{
        start.date <- as_date(temp.nextdate) + days(2)
        next.obs <- temp.nextobs
      }
    }
    
    log.ecls$pct_obscured[log.ecls$pct_obscured >= 1]  <- 1
    log.ecls$pct_obscured <- scales::percent(log.ecls$pct_obscured, accuracy = 0.1)
    #https://stackoverflow.com/questions/21909826/r-shiny-open-the-urls-from-rendertable-in-a-new-tab
    log.ecls$Eclipse_Map <- paste0("link to [<a href='",  
                                   log.ecls$Eclipse_Map,
                                   "' target='_blank'>map</a>]")
    log.ecls
  }, 
  sanitize.text.function = function(x) x
  )
  
  
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
  
  output$qr_url <- renderPlot({
    qr_app <- qrcode::qr_code(x = "https://tim-bender.shinyapps.io/shiny_all_eclipses/", 
                              ecl = "H")
    qr_app_logo <- add_logo(qr_app, 
                            logo = "www/QRLOGO.jpg")
    plot(qr_app_logo)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
