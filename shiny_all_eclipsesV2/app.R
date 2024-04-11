
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
                          h2("Where Am I?"),
                          tags$p("Click the button to get your location"),
                          geoloc::button_geoloc("myBtn", "Get my Location"),
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
  
  output$dtab <- renderTable({
    mtcars
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
