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
          shiny::textInput(inputId = "addr_in", 
                           label = "Enter Address", 
                           value = sample(x = c("1600 Pennsylvania Ave, Washington, DC",      
                                                "4790 W 16th St, Indianapolis, IN"), 
                                          size = 1)),
          actionButton(inputId = "cxy_go", 
                       label   = "SEARCH ADDRESS")
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
