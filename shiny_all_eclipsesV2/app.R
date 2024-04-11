
library(shiny)


ui <- fluidPage(
    titlePanel("<title>"),
    sidebarLayout(
        sidebarPanel(
            fluidRow("sidebarPanel")
        ),

        mainPanel(
           fluidRow("mainPanel")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   
}

# Run the application 
shinyApp(ui = ui, server = server)
