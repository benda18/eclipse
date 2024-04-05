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


# Define UI 
ui <- fluidPage(
  
  # Application title
  titlePanel("Find the Next Solar and Lunar Eclipse for a Location"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        fluidRow("Find the next Solar and Lunar eclipses for any US mailing address. Enter an address and a date to search from below.")
      ),
      shiny::textInput(inputId = "addr_in", 
                       label = "Enter Address", 
                       #value = "6880 Springfield Xenia Rd, Yellow Springs, OH"),
                       value = sample(x = c("1600 Pennsylvania Ave, Washington, DC",      
                                            "1060 W Addison, Chicago IL",               
                                            #"932 Zion – Mount Carmel Hwy, Springdale, UT", 
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
      # shiny::radioButtons(inputId = "radio_obsc",
      #                     label = "Search Radius", 
      #                     choices = list("At Address" = 1, 
      #                                    "~1 hour drive" = 0.99, 
      #                                    "~2 hour drive" = 0.98)),
      shiny::dateInput(inputId = "in_startdate", 
                       label = "Search From Date", 
                       value = Sys.Date(), 
                       min = ymd(10000101), #ymd(18500101), 
                       max = ymd(30001231)), #ymd(21500101)),
      actionButton(inputId = "cxy_go", 
                   label   = "SEARCH"), 
      wellPanel(
        fluidRow("Developed by Tim Bender"), 
        fluidRow(uiOutput("tab.linkedin")),
        fluidRow(uiOutput("tab.github")), 
        fluidRow("Other Work Examples You Might Enjoy:"), 
        fluidRow("[COMING SOON]")
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      wellPanel(
        wellPanel(
          fluidRow(textOutput(outputId = "addr_input")), 
          fluidRow(textOutput(outputId = "addr_output"))
        ),
        
        wellPanel(
          fluidRow(div(h4(strong("NEXT SOLAR ECLIPSE")))),
          fluidRow(tableOutput(outputId = "return_nextSOL")), 
           ),
        wellPanel(
          fluidRow(div(h4(strong("NEXT LUNAR ECLIPSE")))),
          fluidRow(tableOutput(outputId = "return_nextLUN"))
        ),
        wellPanel(
          fluidRow(div(h4(strong("A NOTE ON CALENDAR CALCULATIONS")))),
          fluidRow("The modern Gregorian calendar was adopted beginning in AD 1582.  Attempting to reconcile earlier historical events with these types of astronomical calculations at minimum requires additional verification. The scientific effort to perfect the calendar has caused at least 1 country's Olympic delegation to arrive at the contest 2 weeks late, entirely missing out on participating in some events (https://www.si.com/extra-mustard/2013/12/30/the-extra-mustard-trivia-hour-when-a-calendar-defeated-russia-in-the-1908-olympics)." )
        ), 
        wellPanel(
          fluidRow(strong("DONATIONS - help cover hosting costs")), 
          #fluidRow("This tool was created for fun for the enjoyment and use of others, and was built upon the work of others who came before me. There is montly cost to keep it live for people to use, so if you want to donate to help cover that cost or even a little extra I would appreciate it, but do not expect it"), 
          fluidRow("Venmo: @Tim_J_Bender")
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  le_type <- function(mag_u){
    # total 
    tot_ecl <- mag_u >= 1
    # penumbral
    pen_ecl <- mag_u < 0
    # partial
    par_ecl <- !xor(tot_ecl,pen_ecl)
    
    out <- c("Total Lunar" = tot_ecl, 
             "Penumbral Lunar" = pen_ecl, 
             "Partial Lunar" = par_ecl)
    
    out <- out[out == T]
    out <- names(out)
    return(out)
  }
  
  get_addr_input <- eventReactive(input$cxy_go, {
    input$addr_in
  })
  
  output$addr_input <- renderText({
    paste("Address Searched:", get_addr_input(), sep = " ", collapse = " ")
  })
  
  get_cxyinfo <- eventReactive(input$cxy_go, {
    censusxy::cxy_oneline(address = input$addr_in)
  })
  
  output$addr_output <- renderText({
    get.addr   <- get_cxyinfo()
    paste("Address Returned:", 
          unlist(unname(get.addr["matchedAddress"])), 
          sep = " ", collapse = " ")
  })
  
  get_nextLUN <- eventReactive(eventExpr = input$cxy_go, {{
    start.date <- input$in_startdate
    get.addr   <- get_cxyinfo()
    var.lon    <- unlist(unname(get.addr["coordinates.x"])) # -78.9
    var.lat    <- unlist(unname(get.addr["coordinates.y"])) #  36.0
    
    a.date.ju <- swephR::swe_utc_to_jd(year = year(start.date), 
                                       month = lubridate::month(start.date), 
                                       day   = mday(start.date), 
                                       houri = 0, 
                                       min   = 30, 
                                       sec   = 0, 
                                       gregflag = 1)$dret[2]
    
    when_lunar <- swephR::swe_lun_eclipse_when_loc(jd_start = a.date.ju, 
                                                   ephe_flag = 4, 
                                                   geopos = c(x = var.lon, 
                                                              y = var.lat, 
                                                              z = 10), 
                                                   backward = F)
    
    # https://www.astro.com/swisseph/swephprg.htm#_Toc112948992
    
    next_lunar <- strftime(x = with_tz(ymd_hms(paste(swephR::swe_jdet_to_utc(when_lunar$tret[1], 1), 
                                             sep = "-", collapse = "-"))), 
                           format = "%B %d, %Y at %I:%M%p %Z")
    app_alt_deg <- when_lunar$attr[7]
    
    ecl_type <- le_type(when_lunar$attr[1])
    
    data.frame(Date_Time = next_lunar, 
               Eclipse_Type = ecl_type, 
               Degrees_Above_Horizon = scales::comma(round(app_alt_deg,digits = 0)))
    
  }})
  output$return_nextLUN <- renderTable({
    get_nextLUN()
  })
  
  get_nextSOL <- eventReactive(eventExpr = input$cxy_go, {
    start.date <- input$in_startdate
    get.addr <- get_cxyinfo()
    var.lon <- unlist(unname(get.addr["coordinates.x"])) # -78.9
    var.lat <- unlist(unname(get.addr["coordinates.y"])) #  36.0
    
    
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
      
      ecl_date <- strftime(with_tz(ymd_hms(paste(swephR::swe_jdet_to_utc(when_next$tret[1], 1), 
                                                 sep = "-", collapse = "-"))), 
                           format = "%B %d, %Y at %I:%M%p %Z")
      ecl_type <- ifelse(when_next$attr[2] >= 1, "Total Solar", "Annular Solar")
      ecl_obs  <- when_next$attr[3]
      
      df_ecl <- data.frame(Date_Time = ecl_date, 
                           Eclipse_Type = ecl_type, 
                           Percent_of_Sun_Obscured = scales::percent(floor(ifelse(ecl_obs >=1, 1, ecl_obs)*100)/100))
  })
  
  output$return_nextSOL <- renderTable({
    get_nextSOL()
  })
  
  url.github <- a("GitHub Source Code", 
                  href = "https://github.com/benda18/eclipse/blob/main/shiny_next_eclipse/app.R", 
                  target = "_blank")
  output$tab.github <- renderUI({
    tagList(url.github)
  })
  
  url.linkedin <- a("LinkedIn", 
                    href = "https://www.linkedin.com/in/tim-bender-238870171/", 
                    target = "_blank")
  output$tab.linkedin <- renderUI({
    tagList(url.linkedin)
  })
  
  swe_close()
}

# Run the application 
shinyApp(ui = ui, server = server)
