#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

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
#library(ggmap)

# renv::status()
# renv::snapshot()


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Find Future Eclipse"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        fluidRow("<describe webapp>")
      ),
      shiny::textInput(inputId = "addr_in", 
                       label = "Enter Address", 
                       value = "6880 Springfield Xenia Rd, Yellow Springs, OH"),
      shiny::radioButtons(inputId = "radio_obsc",
                          label = "Search Radius", 
                          choices = list("At Address" = 0, 
                                         "~1 hour drive" = 0.99, 
                                         "~2 hour drive" = 0.98)),
      # shiny::radioButtons(inputId = "radio_bw", 
      #                     label = "Search for Future or Past Eclipse?", 
      #                     choices = list("Future Eclipse(s)" = F,  
      #                                    "Past Eclipse(s)" = T)),
      actionButton(inputId = "cxy_go", 
                   label   = "SEARCH ADDRESS")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      wellPanel(
        fluidRow("title"), 
        fluidRow(textOutput(outputId = "return_nextSOL"))
        
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  get_cxyinfo <- eventReactive(input$cxy_go, {
    censusxy::cxy_oneline(address = input$addr_in)
  })
  
  get_nextSOL <- eventReactive(eventExpr = input$cxy_go, {
    start.date <- Sys.Date()
    get.addr <- get_cxyinfo()
    var.lon <- unlist(unname(get.addr["coordinates.x"]))
    var.lat <- unlist(unname(get.addr["coordinates.y"]))
    
    is_totality <- F
    n <- 0
    while(!is_totality & year(start.date) < 3001){
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
      
      temp.nextobs <- max(when_next$attr[c(1,3)]) # p
      
      ecl_type <- ifelse(temp.nextobs >= 1, "total", "partial")
      
      # DO OUR FILTERING HERE
      
      if(ecl_type == "total"){
        is_totality <- T
      }else{
        start.date <- as_date(temp.nextdate) + days(2)
      }
    }
    
    if(temp.nextobs < 1 & 
       year(start.date) > 3000){
      next.total.eclipse <- "Sometime after the year 3000"
    }else{
      next.total.eclipse <-  strftime(start.date, format = "%B %d, %Y")
    }
    
    glue("Next View of Totality: {next.total.eclipse}")
  })
  
  output$return_nextSOL <- renderText({
    get_nextSOL()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
