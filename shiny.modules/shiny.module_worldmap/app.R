#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
# https://shiny.posit.co/
#

library(shiny)
library(renv)
library(swephR)
library(lubridate)
library(dplyr)
library(tigris)
library(scales)
library(ggplot2)
library(sf)
library(rnaturalearthdata)

# renv::snapshot()

# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("<Clickable World Map>"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      # select continents
      shiny::checkboxGroupInput(inputId = "f_continent",
                                label = "Select Continent(s) to Display on Map",
                                choices = sort(c("North America", "South America", "Africa",
                                            "Asia", "Europe", "Antarctica", "Oceania")),
                                selected = c("North America", "South America", "Africa",
                                             "Asia", "Europe", 
                                             #"Antarctica", 
                                             "Oceania")),
      wellPanel(
        fluidRow("Find the next Solar and Lunar eclipses by clicking on the map")
      ),
      wellPanel(
        fluidRow(textOutput(outputId = "lon_id")), 
        fluidRow(textOutput(outputId = "lat_id"))
      )
      
    ),
    mainPanel(
      wellPanel(
        plotOutput(outputId = "world_map", 
                   click = clickOpts(id = "plot_click"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  data("countries110")
  
  # print lon and lat inputs----
  output$lon_id <- renderText({
    unlist(input$plot_click["x"])
  })
  output$lat_id <- renderText({
    unlist(input$plot_click["y"])
  })
  
  output$world_map <- renderPlot({
    ggplot() + 
      geom_sf(data = countries110[countries110$continent %in% input$f_continent,],
              aes(fill = continent), color = "#363838") +
      theme(panel.background = element_rect(fill = "#9ce4ff"), 
            legend.position = "none", 
            panel.grid = element_blank())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
