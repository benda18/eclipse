library(shiny)
library(renv)
library(ggplot2)
library(rnaturalearthdata)
library(sf)
library(censusxy)
library(dplyr)
#library(remotes)
library(geoloc)
library(leaflet)

#renv::snapshot()

#remotes::install_github("ColinFay/geoloc")

ui <- navbarPage(title = "<Title>", 
                 id = "tabs",
                 tabPanel(title = "Search by Address (USA only)",
                          shiny::textInput(inputId = "cxy_in", 
                                           label = "address")),
                 tabPanel(
                   title = "Search by Map Click", 
                   shiny::plotOutput(outputId = "world_map",
                                     click = clickOpts(id = "plot_click")), 
                   wellPanel(
                     fluidRow(
                       textOutput(outputId = "lon_id")
                     ), 
                     fluidRow(
                       textOutput(outputId = "lat_id")
                     )
                   )
                 ),
                 tabPanel("Search by Current Location", 
                          h2("Where Am I?"),
                          tags$p("Click the button to get your location"),
                          geoloc::button_geoloc("myBtn", "Get my Location"),
                          tags$br(),
                          leafletOutput("lf"),
                          # actionButton("use_clik_loc", "Check loc"), 
                          # wellPanel(
                          #   fluidRow(
                          #     textOutput(outputId = "lon_id")
                          #   ), 
                          #   fluidRow(
                          #     textOutput(outputId = "lat_id")
                          #   )
                          # )
                          ),
                 tabPanel("Search by Lon/Lat"),
                 mainPanel(
                   
                 )
)


server <- function(input, output, session) {
  data("countries50")
  
  # observeEvent(input$use_clik_loc, {
  #   print(isolate(as.data.frame(input$map_click)))
  # })
  
  # geoloc 
  output$lf <- renderLeaflet({
    req(input$myBtn_lon)
    req(input$myBtn_lat)
    leaflet() %>%
      addTiles() %>%
      setView(as.numeric(input$myBtn_lon), as.numeric(input$myBtn_lat), zoom = 17) %>%
      addMarkers(as.numeric(input$myBtn_lon), as.numeric(input$myBtn_lat), label = "You're here!")
  })
  
  
  # print lon and lat inputs----
  output$lon_id <- renderText({
    unlist(input$plot_click["x"])
  })
  output$lat_id <- renderText({
    unlist(input$plot_click["y"])
  })
  
  output$world_map <- renderPlot({
    ggplot() + 
      geom_sf(data = countries50,#[countries50$continent %in% input$f_continent,],
              #aes(fill = continent), 
              fill = "white",
              color = "#363838") +
      theme(panel.background = element_rect(fill = "#9ce4ff"), 
            legend.position = "none", 
            panel.grid = element_blank())
  })
}

shinyApp(ui, server)
