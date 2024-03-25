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
      shiny::radioButtons(inputId = "radio_bw", 
                          label = "Search for Future or Past Eclipse?", 
                          choices = list("Future Eclipse(s)" = F,  
                                         "Past Eclipse(s)" = T)),
      actionButton(inputId = "cxy_go", 
                   label   = "SEARCH ADDRESS")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      wellPanel(
        fluidRow("title")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
