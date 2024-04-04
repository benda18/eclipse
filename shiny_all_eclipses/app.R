#
# This is a Shiny web application.
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("<title>"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fluidRow("firstrow")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          fluidRow("FirstRow")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
