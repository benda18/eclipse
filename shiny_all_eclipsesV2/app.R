
library(shiny)
library(leaflet)
library(geoloc)
library(swephR)
library(lubridate)
library(dplyr)
library(renv)
library(leaflet)


#renv::snapshot()

ui <- navbarPage(title = "<Title>", 
                 id = "tabs",
                 tabPanel("Search by Current Location", 
                          #h2("Where Am I?"),
                          strong(tags$p("Click the button")),
                          geoloc::button_geoloc("myBtn", "Show my Location"),
                          tags$br(),
                          leafletOutput("map", 
                                        width = "100%"),
                          wellPanel(
                            shiny::tableOutput("vxy")
                          ), 
                          wellPanel(
                            shiny::tableOutput("dtab")
                          )
                 )
                 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$vxy <- shiny::renderTable({
    data.frame("lon" = input$myBtn_lon, 
               "lat" = input$myBtn_lat)
  })
  
  output$map <- renderLeaflet({
    req(input$myBtn_lon)
    req(input$myBtn_lat)
    leaflet() %>%
      addTiles() %>%
      setView(as.numeric(input$myBtn_lon), as.numeric(input$myBtn_lat), zoom = 17) %>%
      addMarkers(as.numeric(input$myBtn_lon), as.numeric(input$myBtn_lat), label = "You're here!")
  })
  
  # MASTER OUTPUT TABLE----
   output$dtab <- renderTable({
    # UNIVERSAL VARIABLES
    lon.in     <- req(input$myBtn_lon)
    lat.in     <- req(input$myBtn_lat)
    # lon.in     <- runif(-180,180,n=1)
    # lat.in     <- runif(-90, 90, n=1)
    greg_dt.in <- Sys.time()
    loctz.in   <- "America/New_York"
    
    # Transformations----
    greg_utcdt.in <- greg_dt.in |> with_tz(tzone = "UTC")
    
    jul_utcdt.in  <- swephR::swe_utc_to_jd(year     = year(greg_utcdt.in), 
                                           month    = lubridate::month(greg_utcdt.in), 
                                           day      = mday(greg_utcdt.in), 
                                           houri    = hour(greg_utcdt.in), 
                                           min      = minute(greg_utcdt.in), 
                                           sec      = second(greg_utcdt.in), 
                                           gregflag = 1)$dret[2] 
    
    
    # SWEPHR FUNS----
    # find the next eclipse for a given geographic position;
    when_next.sol <- swe_sol_eclipse_when_loc(jd_start  = jul_utcdt.in, 
                                              ephe_flag = 4, 
                                              geopos    = c(lon.in, 
                                                            lat.in, 
                                                            10), 
                                              backward  = FALSE) 
    
    # find the next eclipse globally (including type of eclipse);
    ecl_total   <- swe_sol_eclipse_when_glob(jd_start  = jul_utcdt.in,
                                             ephe_flag = 4, 
                                             ifltype   = SE$ECL_TOTAL, 
                                             backward  = FALSE)
    ecl_annular <- swe_sol_eclipse_when_glob(jd_start  = jul_utcdt.in,
                                             ephe_flag = 4, 
                                             ifltype   = SE$ECL_ANNULAR,
                                             backward  = FALSE)
    ecl_partial <- swe_sol_eclipse_when_glob(jd_start  = jul_utcdt.in,
                                             ephe_flag = 4, 
                                             ifltype   = SE$ECL_PARTIAL,
                                             backward  = FALSE)
    ecl_hybrid  <- swe_sol_eclipse_when_glob(jd_start  = jul_utcdt.in,
                                             ephe_flag = 4, 
                                             ifltype   = SE$ECL_ANNULAR_TOTAL,
                                             backward  = FALSE)
    
    ecl_type <- c("Total Eclipse", "Annular", 
                  "Partial", "Hybrid")[which(abs(c(ecl_total$tret[2] - when_next.sol$tret[2],
                                                   ecl_annular$tret[2] - when_next.sol$tret[2],
                                                   ecl_partial$tret[2] - when_next.sol$tret[2],
                                                   ecl_hybrid$tret[2] - when_next.sol$tret[2])) == 
                                               min(abs(c(ecl_total$tret[2] - when_next.sol$tret[2],
                                                         ecl_annular$tret[2] - when_next.sol$tret[2],
                                                         ecl_partial$tret[2] - when_next.sol$tret[2],
                                                         ecl_hybrid$tret[2] - when_next.sol$tret[2]))))]
    
    
    
    # compute the geographic location of a solar eclipse for a given tjd;
    
    jul_utc_ecl.begin <- swe_sol_eclipse_when_glob(jd_start  = jul_utcdt.in,
                                                   ephe_flag = 4, 
                                                   ifltype   = 0, # 0 = any eclipse 
                                                   backward  = FALSE)$tret[c(3:4)]
    
    
    swe_sol_eclipse_where(jd_ut     = min(jul_utc_ecl.begin), 
                          ephe_flag = 4) 
    swe_sol_eclipse_where(jd_ut     = max(jul_utc_ecl.begin), 
                          ephe_flag = 4) 
    
    # compute attributes of a solar eclipse for a given tjd, geographic longitude,
    # latitude and height.
    swe_sol_eclipse_how(jd_ut     = jul_utcdt.in, 
                        ephe_flag = 4, 
                        geopos    = c(lon.in, 
                                      lat.in, 
                                      10)) 
    
    # find the next lunar eclipse for a given geographic position;
    swe_lun_eclipse_when_loc(jd_start  = jul_utcdt.in, 
                             ephe_flag = 4, 
                             geopos    = c(lon.in, 
                                           lat.in, 
                                           10), 
                             backward  = FALSE) 
    
    # find the next lunar eclipse;
    swe_lun_eclipse_when(jd_start  = jul_utcdt.in,
                         ephe_flag = 4, 
                         ifltype   = 0, # SE_ECL_TOTAL, SE_ECL_PARTIAL, or SE_ECL_PENUMBRAL
                         backward  = FALSE) 
    
    # compute the attributes of a lunar eclipse for a given tjd.
    swe_lun_eclipse_how(jd_ut     = jul_utcdt.in, 
                        ephe_flag = 4,
                        geopos    = c(lon.in, 
                                      lat.in, 
                                      10)) 
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
