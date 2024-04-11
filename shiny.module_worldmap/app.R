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
#library(shiny)
#library(jpeg)
library(swephR)
library(lubridate)
library(dplyr)
library(tigris)
#library(censusxy)
library(scales)
library(ggplot2)
library(sf)
#library(glue)
#library(rsconnect)
#library(qrcode)
library(rnaturalearthdata)
library(maps)

# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("title"),
  
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
      
    ),
    mainPanel(
      plotOutput(outputId = "world_map")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  data("countries50", "world.cities")
  
  output$world_map <- renderPlot({
    ggplot() + 
      geom_sf(data = countries50[countries50$continent %in% input$f_continent,],
              aes(fill = continent), color = "#363838")+
      theme(panel.background = element_rect(fill = "#9ce4ff"), 
            legend.position = "none", 
            panel.grid = element_blank())+
      geom_point(data = slice_max(world.cities,
                                  order_by = pop, 
                                  prop = 0.0125), 
                 aes(x = long, y = lat))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
