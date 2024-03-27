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
#https://stackoverflow.com/questions/49190820/create-data-set-from-clicks-in-shiny-ggplot

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Find the Next Solar and Lunar Eclipse for a Location"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        fluidRow("Find the next Solar and Lunar eclipses by clicking on the map")
      ),
      wellPanel(
        fluidRow(textOutput(outputId = "lon_id")), 
        fluidRow(textOutput(outputId = "lat_id"))
      ),
      # shiny::numericInput(inputId = "lon_in", 
      #                     label = "Longitude(x)", 
      #                     value = -79, 
      #                     min = -180, 
      #                     max = 180),
      # shiny::numericInput(inputId = "lat_in", 
      #                     label = "Latitude(y)", 
      #                     value = 35.5, 
      #                     min = -90, 
      #                     max = 90),
      # [hold for] obscuration filter----
      shiny::radioButtons(inputId = "radio_obsc",
                          label = "Search Radius",
                          choices = list("At Address (i.e. Within Totality)" = 1,
                                         "~1 hr drive (~99+% Sun Obscured)" = 0.99,
                                         "~2 hr drive (~98+% Sun Obscured)" = 0.98)),
      shiny::dateInput(inputId = "in_startdate", 
                       label = "Search From Date", 
                       value = Sys.Date(), 
                       min = ymd(10000101), 
                       max = ymd(30001231)), 
      wellPanel(
        fluidRow("Developed by Tim Bender"), 
        fluidRow(uiOutput("tab.linkedin")),
        fluidRow(uiOutput("tab.github")), 
        fluidRow("Other Work Examples You Might Enjoy:"), 
        fluidRow("[COMING SOON]")
      ),
      wellPanel(
        fluidRow(div(h4(strong("A NOTE ON CALENDAR CALCULATIONS")))),
        fluidRow("Reconciling historical events with these types of astronomical calculations at minimum requires additional verification." )
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      wellPanel(
        wellPanel(
          plotOutput(outputId = "map_xy", 
                     click = clickOpts(id = "plot_click"))
        ),
        wellPanel(
          fluidRow(div(h4(strong("NEXT SOLAR ECLIPSE")))),
          fluidRow(tableOutput(outputId = "return_nextSOL")), 
          fluidRow(div(h4(strong("NEXT LUNAR ECLIPSE")))),
          fluidRow(tableOutput(outputId = "return_nextLUN"))
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  usa.states <- readRDS("usastates.rds")
  
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
  
  
  # print lon and lat inputs----
  output$lon_id <- renderText({
    #input$lon_in
    unlist(input$plot_click["x"])
  })
  output$lat_id <- renderText({
    #input$lat_in
    unlist(input$plot_click["y"])
  })
  
  output$return_nextLUN <- renderTable({
    start.date <- input$in_startdate
    # var.lon    <- input$lon_in
    var.lon    <- as.numeric(unlist(input$plot_click["x"]))
    # var.lat    <- input$lat_in
    var.lat    <- as.numeric(unlist(input$plot_click["y"]))
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
  })
  
  output$return_nextSOL <- renderTable({
    start.date <- input$in_startdate
    # var.lon    <- input$lon_in
    var.lon    <- as.numeric(unlist(input$plot_click["x"]))
    # var.lat    <- input$lat_in
    var.lat    <- as.numeric(unlist(input$plot_click["y"]))
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
  
  url.github <- a("GitHub Source Code", 
                  href = "https://github.com/benda18/eclipse/blob/main/shiny_next_eclipse/app.R", 
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
  
  output$map_xy <- renderPlot({
    plot <- ggplot() + 
      geom_sf(data = usa.states)+
      theme_void()
    
    plot
  })
  
  
  swe_close()
}

# Run the application 
shinyApp(ui = ui, server = server)
