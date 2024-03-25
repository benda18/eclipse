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
      # shiny::radioButtons(inputId = "radio_obsc",
      #                     label = "Search Radius", 
      #                     choices = list("At Address" = 1, 
      #                                    "~1 hour drive" = 0.99, 
      #                                    "~2 hour drive" = 0.98)),
      # shiny::radioButtons(inputId = "radio_bw", 
      #                     label = "Search for Future or Past Eclipse?", 
      #                     choices = list("Future Eclipse(s)" = F,  
      #                                    "Past Eclipse(s)" = T)),
      shiny::dateInput(inputId = "in_startdate", 
                       label = "Search From Date", 
                       value = Sys.Date(), 
                       min = ymd(18500101), 
                       max = ymd(21500101)),
      actionButton(inputId = "cxy_go", 
                   label   = "SEARCH")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      wellPanel(
        fluidRow("title"), 
        #fluidRow(textOutput(outputId = "return_nextSOL")),
        fluidRow(tableOutput(outputId = "return_nextSOL"))
        
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
    start.date <- input$in_startdate
    get.addr <- get_cxyinfo()
    var.lon <- unlist(unname(get.addr["coordinates.x"])) # -78.9
    var.lat <- unlist(unname(get.addr["coordinates.y"])) #  36.0
    
    
    #df_ecl <- NULL
    #ecl_found <- F
    #for(i in 1:10){ # just look for the next 5 
    #while(start.date <= ymd(21500101)){
    #while(!ecl_found){
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
      
      ecl_date <- strftime(as_date(ymd_hms(paste(swephR::swe_jdet_to_utc(when_next$tret[1], 1), 
                                                 sep = "-", collapse = "-"))), 
                           format = "%B %d, %Y")
      ## filter for obscuration
      #if(when_next$attr[3] >= input$radio_obsc){
        ecl_type <- ifelse(when_next$attr[2] >= 1, "Total Solar", "Annular Solar")
        ecl_obs  <- when_next$attr[3]
        
        df_ecl <- data.frame(date = ecl_date, 
                                   type = ecl_type, 
                                   obscuration = scales::percent(floor(ifelse(ecl_obs >=1, 1, ecl_obs)*100)/100))
      #}
      #start.date <- ecl_date + days(2)
      #}
    
    
      #df_ecl$date <- as_date(df_ecl$date)
     
  })
  
  # output$return_nextSOL <- renderText({
  #   get_nextSOL()
  # })
  output$return_nextSOL <- renderTable({
    get_nextSOL()
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
