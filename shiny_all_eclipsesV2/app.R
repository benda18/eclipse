#
# This is a Shiny web application.
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(readr)
library(shiny)
library(jpeg)
library(swephR)
library(lubridate)
library(dplyr)
library(censusxy)
library(scales)
library(ggplot2)
library(renv)
library(glue)
library(qrcode)
library(leaflet)
library(geoloc)




ui <- fluidPage(
  
  # Application title
  titlePanel("Future Solar and Lunar Eclipses Visible from Your Current Location", ),
  sidebarLayout(
    sidebarPanel(
      shiny::selectInput(inputId = "n_fut_yrs",
                         label = "Years to Look into the Future:",
                         choices = c("1 year" = 1, 
                                     "5 years" = 5, 
                                     "10 years" = 10,
                                     "25 years" = 25, 
                                     "50 years" = 50,
                                     "75 years" = 75),
                         selected = 25,
                         multiple = F),
      shiny::sliderInput(inputId = "obs_in", 
                         label = "Minimum Solar Eclipse Obscuration Cutoff:", 
                         min = 0, max = 100, value = 10, step = 5, 
                         post = "%"),
      shiny::radioButtons(inputId = "sel_type", 
                          label = "Select Type(s) of Eclipse to Show", 
                          choices = c("All Eclipses" = "Solar|Lunar", 
                                      "Solar Only" = "Solar", 
                                      "Lunar Only" = "Lunar"), 
                          selected = "Solar"),
      shiny::checkboxInput("cb_total.ecl", 
                           value = F,
                           label = "Show Only Total Eclipses"), 
      shiny::checkboxInput("cb_totality", 
                           value = F, 
                           label = "Show Only when in Path of Totality"),
      geoloc::button_geoloc("myBtn", ("Click to Start")),
      leafletOutput("lf_map"),
      
      
    ),
    
    mainPanel(
      # BLOCK RESOURCES MAIN PANEL----
      wellPanel(
        fluidRow(strong(uiOutput("tab.linkedin"))),
        fluidRow(strong(uiOutput("tab.github"))),
        fluidRow(strong(uiOutput("tab.venmo"))),
        fluidRow("Special thanks to reddit users u/danielsixfive and u/QuackingUp23")
      ),
      #/BRMP
      
      wellPanel(
        fluidRow(strong("Data Table Will Load Below (may take a moment)")),
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

# Define server logic
server <- function(input, output) {
  
  ewlun_url <- function(ecl_date, 
                        ecltype){
    require(glue)
    require(lubridate)
    w.year  <- year(ecl_date)
    w.month <- lubridate::month(ecl_date,label=T,abbr=T)
    w.mday  <- mday(ecl_date)
    w.mday  <- ifelse(nchar(w.mday) == 1,
                      paste("0", w.mday, sep = "", collapse = ""),
                      w.mday)
    w.cenA  <- floor(w.year/100)*100+1
    w.cenB  <- w.cenA + 99
    
    ecltype <- ifelse(ecltype %in% c("Penumbral", 
                                     "penumbral"), "NPenumbral", ecltype)
    et <- toupper(substr(ecltype,1,1))
    glue("https://eclipsewise.com/lunar/LEprime/{w.cenA}-{w.cenB}/LE{w.year}{w.month}{w.mday}{et}prime.html")
    #glue("https://eclipsewise.com/oh/ec{year(ecl_date)}.html#LE{year(ecl_date)}{lubridate::month(ecl_date,abbr=T,label=T)}{mday(ecl_date)}{et}")
    
  }
  ewlun_url(mdy("Aug 28, 2026"),"Penumbral")
  
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
    start.date      <- with_tz(Sys.Date(), tzone = "America/New_York") #input$date_in 
    max.year        <- year(start.date) + as.numeric(input$n_fut_yrs)
    
    # do work----
    var.lon <- input$myBtn_lon 
    var.lat <- input$myBtn_lat 
    
    ####
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
      
      # get next solar eclipse----
      when_next <- swe_sol_eclipse_when_loc(jd_start = a.date.ju, 
                                            ephe_flag = 4, 
                                            geopos = c(x = var.lon, 
                                                       y = var.lat, 
                                                       z = 10), 
                                            backward = F)
      
      # search solar eclipse type
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
      
      # get next lunar eclipse----
      when_next.lun <- swe_lun_eclipse_when_loc(jd_start = a.date.ju, 
                                                ephe_flag = 4, 
                                                geopos = c(x = var.lon, 
                                                           y = var.lat, 
                                                           z = 10), 
                                                backward = F)
      # get next lunar eclipse type
      
      ecl_total.lun <- swe_lun_eclipse_when(jd_start = when_next.lun$tret[3]-1, 
                                            ifltype = SE$ECL_TOTAL,
                                            ephe_flag = 4, 
                                            backward = F)
      ecl_penumbral.lun <- swe_lun_eclipse_when(jd_start = when_next.lun$tret[3]-1, 
                                                ifltype = SE$ECL_PENUMBRAL,
                                                ephe_flag = 4, 
                                                backward = F)
      ecl_partial.lun <- swe_lun_eclipse_when(jd_start = when_next.lun$tret[3]-1, 
                                              ifltype = SE$ECL_PARTIAL,
                                              ephe_flag = 4, 
                                              backward = F)
      
      
      ecl_type222.lun <- c("Total Eclipse", "Penumbral", 
                           "Partial")[which(abs(c(ecl_total.lun$tret[3] - when_next.lun$tret[3],
                                                  ecl_penumbral.lun$tret[3] - when_next.lun$tret[3],
                                                  ecl_partial.lun$tret[3] - when_next.lun$tret[3])) == 
                                              min(abs(c(ecl_total.lun$tret[3] - when_next.lun$tret[3],
                                                        ecl_penumbral.lun$tret[3] - when_next.lun$tret[3],
                                                        ecl_partial.lun$tret[3] - when_next.lun$tret[3]))))]
      # temp manual fix for penumbral eclipses
      if(length(ecl_type222.lun) == 3){
        ecl_type222.lun <- "Penumbral"
      }
      
      
      # NEXT DATE
      temp.nextdate <- ymd_hms(paste(swephR::swe_jdet_to_utc(when_next$tret[1], 1), 
                                     sep = "-", collapse = "-"))
      temp.nextdate.lun <- ymd_hms(paste(swephR::swe_jdet_to_utc(when_next.lun$tret[1], 1), 
                                     sep = "-", collapse = "-"))
      
      #temp.nextobs <- max(when_next$attr[c(1,3)]) # p
      temp.nextobs <- when_next$attr[c(3)]
      
      log.ecls <- rbind(log.ecls,
                        data.frame(Date = strftime(x = temp.nextdate, 
                                                   format = "%b %d, %Y", 
                                                   tz = "America/New_York"),
                                   Type = "Solar",
                                   Sub_Type = ecl_type222,
                                   Obscuration = temp.nextobs, 
                                   See_Also = eclipsewise_url(ecl_date = temp.nextdate, 
                                                                 ecltype = ecl_type222)))
      log.ecls <- rbind(log.ecls,
                        data.frame(Date = strftime(x = temp.nextdate.lun, 
                                                   format = "%b %d, %Y", 
                                                   tz = "America/New_York"),
                                   Type = "Lunar",
                                   Sub_Type = ecl_type222.lun,
                                   Obscuration = NA, 
                                   See_Also = ewlun_url(ecl_date = temp.nextdate.lun, 
                                                           ecltype  = ecl_type222.lun)))
      
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
        #is_totality <- T
        # next.obs <- temp.nextobs
        # start.date <- as_date(temp.nextdate)
      }else{
        start.date <- as_date(min(c(temp.nextdate.lun, 
                                temp.nextdate))) + days(2)
        next.obs <- temp.nextobs
      }
    }
    
    log.ecls$Obscuration[log.ecls$Obscuration >= 1 & !is.na(log.ecls$Obscuration)]  <- 1
    log.ecls <- log.ecls[(log.ecls$Obscuration*100) >= input$obs_in | 
                           is.na(log.ecls$Obscuration),]
    log.ecls$Obscuration <- scales::percent(log.ecls$Obscuration, accuracy = 0.1)
    #https://stackoverflow.com/questions/21909826/r-shiny-open-the-urls-from-rendertable-in-a-new-tab
    log.ecls$See_Also <- paste0("[<a href='",  
                                   log.ecls$See_Also,
                                   "' target='_blank'>see eclipse path</a>]")
    
    log.ecls$See_Also[log.ecls$Type == "Lunar"] <-
      gsub(pattern = "eclipse path", 
           replacement = "eclipse info", 
           x = log.ecls$See_Also[log.ecls$Type == "Lunar"])
    
    
    # checkbox_totaleclipse
    if(input$cb_total.ecl){
      log.ecls <- log.ecls[log.ecls$Sub_Type == "Total Eclipse",]
    }
    # checkbox_totality
    if(input$cb_totality){
      log.ecls <- log.ecls[log.ecls$Obscuration == "100.0%" & 
                             log.ecls$Type == "Solar",]
    }
    
    log.ecls <- log.ecls[!duplicated(log.ecls),]
    log.ecls[grepl(pattern = input$sel_type, 
                   x = log.ecls$Type),]
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
                  href = "https://github.com/benda18/eclipse/blob/main/shiny_all_eclipsesV2/app.R", 
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
