#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

#https://shiny.posit.co/r/reference/shiny/latest/

# Define UI for application 
ui <- fluidPage(

  titlePanel("Application Title"),

  navlistPanel(
    "Header",
    tabPanel("First"),
    tabPanel("Second"),
    tabPanel("Third")
  )
)

# Define server logic 
server <- function(input, output) {

    
}

# Run the application 
shinyApp(ui = ui, server = server)
