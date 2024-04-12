#
# This is a Shiny web application.
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(jpeg)
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
library(leaflet)
library(geoloc)




ui <- fluidPage(
  
  # Application title
  titlePanel("75 Years of Solar Eclipses Visible from Your Current Location", ),
  sidebarLayout(
    sidebarPanel(
      geoloc::button_geoloc("myBtn", "Click to Start"),
      leafletOutput("lf_map"),
      shiny::dateInput(inputId = "date_in", 
                       label = "Search-From Date", 
                       value = Sys.Date(), 
                       min   = "1000-01-01", 
                       max   = "2999-12-31", 
                       format = "MM dd, yyyy"),
      shiny::checkboxInput("cb_total.ecl", 
                           value = F,
                           label = "Show Only Total Eclipses"), 
      shiny::checkboxInput("cb_totality", 
                           value = F, 
                           label = "Show Only when in Path of Totality")
      
    ),
    
    mainPanel(
      # BLOCK RESOURCES MAIN PANEL----
      wellPanel(
        # fluidRow(strong("DEVELOPED BY")), 
        fluidRow(strong(uiOutput("tab.linkedin"))),
        #fluidRow(strong("SPECIAL ASSISTANCE FROM")),
        #fluidRow(strong("SOURCES")),
        fluidRow(strong(uiOutput("tab.github"))),
        #fluidRow(strong("Help Cover ")), 
        fluidRow(strong(uiOutput("tab.venmo"))),
        fluidRow("Special thanks to reddit users u/danielsixfive and u/QuackingUp23")
      ),
      #/BRMP
      
      wellPanel(
        fluidRow("The table below shows the next 75 years of solar eclipses visible from this location."),
      ),
      shiny::tableOutput(outputId = "logtable"),
      shiny::plotOutput(outputId = "qr_url", 
                        height = "200px"),
      wellPanel(
        fluidRow(strong("OTHER ECLIPSE WEBAPPS")), 
        fluidRow(uiOutput("tab.PT")), 
        fluidRow(uiOutput("tab.NE"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  eclipsewise_url <- function(ecl_date,
                              ecltype = c("Total Eclipse", 
                                          "Annular", 
                                          "Partial", 
                                          "Hybrid")){
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
  
  output$lf_map <- renderLeaflet({
    req(input$myBtn_lon)
    req(input$myBtn_lat)
    leaflet() %>%
      addTiles() %>%
      setView(as.numeric(input$myBtn_lon), as.numeric(input$myBtn_lat), zoom = 8) %>%
      addMarkers(as.numeric(input$myBtn_lon), as.numeric(input$myBtn_lat), label = "You're here!")
  })
  
  output$logtable <- shiny::renderTable({
    # vars----
    start.date      <- input$date_in 
    max.year        <- year(start.date) + 75
    #min_obsc        <- 1 
    
    # do work----
    var.lon <- input$myBtn_lon 
    var.lat <- input$myBtn_lat 
    
    ####
    is_totality <- F
    n <- 0
    
    log.ecls <- NULL
    req(input$myBtn_lon)
    req(input$myBtn_lat)
    while(year(start.date) < max.year){
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
      # ecl_typecheck0 <- min(abs(c(ecl_total$tret[2] - when_next$tret[2],
      #                         ecl_annular$tret[2] - when_next$tret[2],
      #                         ecl_partial$tret[2] - when_next$tret[2],
      #                         ecl_hybrid$tret[2] - when_next$tret[2])))
      
      
      
      # NEXT DATE
      temp.nextdate <- ymd_hms(paste(swephR::swe_jdet_to_utc(when_next$tret[1], 1), 
                                     sep = "-", collapse = "-"))
      
      temp.nextobs <- max(when_next$attr[c(1,3)]) # p
      
      log.ecls <- rbind(log.ecls,
                        data.frame(Date = strftime(x = temp.nextdate, 
                                                   format = "%b %d, %Y", 
                                                   tz = "America/New_York"),
                                   Type = ecl_type222,
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
    log.ecls$Eclipse_Map <- paste0("[<a href='",  
                                   log.ecls$Eclipse_Map,
                                   "' target='_blank'>see eclipse path</a>]")
    
    # checkbox_totaleclipse
    if(input$cb_total.ecl){
      log.ecls <- log.ecls[log.ecls$Type == "Total Eclipse",]
    }
    # checkbox_totality
    if(input$cb_totality){
      log.ecls <- log.ecls[log.ecls$pct_obscured == "100.0%",]
    }
    
    log.ecls
  }, 
  sanitize.text.function = function(x) x
  )
  
  
  # RESOURCES----
  url.venmo <- a("Want to help cover hosting costs? Venmo: @Tim_J_Bender", 
                 href = "https://venmo.com/u/Tim_J_Bender", 
                 target = "_blank")
  output$tab.venmo <- renderUI({
    tagList(url.venmo)
  })
  
  url.github <- a("Source Code (Github)", 
                  href = "https://github.com/benda18/eclipse/blob/main/shiny_all_eclipses/app.R", 
                  target = "_blank")
  output$tab.github <- renderUI({
    tagList(url.github)
  })
  
  url.linkedin <- a("Created by Tim Bender (LinkedIn)", 
                    href = "https://www.linkedin.com/in/tim-bender-238870171/", 
                    target = "_blank")
  output$tab.linkedin <- renderUI({
    tagList(url.linkedin)
  })
  #/RESOURCES
  url.AE <- a("* Every Solar Eclipse Visible from Your Address for 75 Years",
              href = "https://tim-bender.shinyapps.io/shiny_all_eclipses/",
              target = "_blank")
  output$tab.AE <- renderUI({
    tagList(url.AE)
  })
  url.PT <- a("* Total Eclipse of April 8, 2024 - What to Expect from Your Location",
              href = "https://tim-bender.shinyapps.io/shiny_eclipse_planner/",
              target = "_blank")
  output$tab.PT <- renderUI({
    tagList(url.PT)
  })
  
  
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
