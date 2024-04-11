#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("<titlePanel>"),

    # Sidebar 
    sidebarLayout(
        sidebarPanel(
            fluidRow("<sidebarPanel>")
        ),

        # Main Panel
        mainPanel(
           fluidRow("<mainPanel>")
        )
    )
)

# Define server logic 
server <- function(input, output) {

    
}

# Run the application 
shinyApp(ui = ui, server = server)
