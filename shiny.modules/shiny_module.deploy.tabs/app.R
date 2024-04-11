library(shiny)
library(renv)
library(ggplot2)
library(rnaturalearthdata)
library(sf)

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
                 tabPanel("Search by Current Location", "This is the bar tab"),
                 tabPanel("Search by Lon/Lat"),
                 # navbarMenu("More",
                 #            tabPanel("Table", "Table page"),
                 #            tabPanel("About", "About page"),
                 #            "------",
                 #            "Even more!",
                 #            tabPanel("Email", "Email page")
                 # )
                 mainPanel(
                   
                 )
)


server <- function(input, output, session) {
  data("countries50")
  
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
