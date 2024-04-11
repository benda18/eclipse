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
library(censusxy)
library(scales)
library(ggplot2)
library(sf)
library(glue)
#library(rsconnect)
#library(qrcode)
library(rnaturalearthdata)

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
                                choices = c("North America", "South America", "Africa",
                                            "Asia", "Europe", "Antarctica", "Oceania"),
                                selected = c("North America", "South America", "Africa",
                                             "Asia", "Europe", "Antarctica", "Oceania")),
      # selectInput(inputId = "f_country", 
      #             label = "Filter by Country (multiple):",
      #             choices = 
      #               list("North America" = c("Belize", "Canada", "Costa Rica", "Cuba", "Dominican Republic", "El Salvador", "Greenland", "Guatemala", "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama", "Puerto Rico", "The Bahamas", "Trinidad and Tobago", "United States of America"), 
      #                    "South America" = c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Falkland Islands", "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela"), 
      #                    "Africa" = c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", "Central African Republic", "Chad", "Democratic Republic of the Congo", "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "eSwatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Ivory Coast", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Republic of the Congo", "Rwanda", "Senegal", "Sierra Leone", "Somalia", "Somaliland", "South Africa", "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda", "Western Sahara", "Zambia", "Zimbabwe"), 
      #                    "Asia" = c("Afghanistan", "Armenia", "Azerbaijan", "Bangladesh", "Bhutan", "Brunei", "Cambodia", "China", "Cyprus", "East Timor", "Georgia", "India", "Indonesia", "Iran", "Iraq", "Israel", "Japan", "Jordan", "Kazakhstan", "Kuwait", "Kyrgyzstan", "Laos", "Lebanon", "Malaysia", "Mongolia", "Myanmar", "Nepal", "North Korea", "Northern Cyprus", "Oman", "Pakistan", "Palestine", "Philippines", "Qatar", "Saudi Arabia", "South Korea", "Sri Lanka", "Syria", "Taiwan", "Tajikistan", "Thailand", "Turkey", "Turkmenistan", "United Arab Emirates", "Uzbekistan", "Vietnam", "Yemen"), 
      #                    "Europe" = c("Albania", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Kosovo", "Latvia", "Lithuania", "Luxembourg", "Moldova", "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", "Republic of Serbia", "Romania", "Russia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom"), 
      #                    "Antarctica" = c("Antarctica"), 
      #                    "Oceania" = c("Australia", "Fiji", "New Caledonia", "New Zealand", "Papua New Guinea", "Solomon Islands", "Vanuatu")), 
      #             selected = NULL, 
      #             multiple = T, 
      #             selectize = T, 
      #             width = NULL, 
      #             size = NULL),
      # # selectInput(inputId = "f_subregion", 
      # #             label = "Filter by Continent and Subregion:",
      # #             choices = 
      # #               list("North America" = c("Belize", "Canada", "Costa Rica", "Cuba", "Dominican Republic", "El Salvador", "Greenland", "Guatemala", "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama", "Puerto Rico", "The Bahamas", "Trinidad and Tobago", "United States of America"), 
      # #                    "South America" = c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Falkland Islands", "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela"), 
      # #                    "Africa" = c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", "Central African Republic", "Chad", "Democratic Republic of the Congo", "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "eSwatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Ivory Coast", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Republic of the Congo", "Rwanda", "Senegal", "Sierra Leone", "Somalia", "Somaliland", "South Africa", "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda", "Western Sahara", "Zambia", "Zimbabwe"), 
      # #                    "Asia" = c("Afghanistan", "Armenia", "Azerbaijan", "Bangladesh", "Bhutan", "Brunei", "Cambodia", "China", "Cyprus", "East Timor", "Georgia", "India", "Indonesia", "Iran", "Iraq", "Israel", "Japan", "Jordan", "Kazakhstan", "Kuwait", "Kyrgyzstan", "Laos", "Lebanon", "Malaysia", "Mongolia", "Myanmar", "Nepal", "North Korea", "Northern Cyprus", "Oman", "Pakistan", "Palestine", "Philippines", "Qatar", "Saudi Arabia", "South Korea", "Sri Lanka", "Syria", "Taiwan", "Tajikistan", "Thailand", "Turkey", "Turkmenistan", "United Arab Emirates", "Uzbekistan", "Vietnam", "Yemen"), 
      # #                    "Europe" = c("Albania", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Kosovo", "Latvia", "Lithuania", "Luxembourg", "Moldova", "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", "Republic of Serbia", "Romania", "Russia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom"), 
      # #                    "Antarctica" = c("Antarctica"), 
      # #                    "Oceania" = c("Australia", "Fiji", "New Caledonia", "New Zealand", "Papua New Guinea", "Solomon Islands", "Vanuatu")), 
      # #             selected = NULL, 
      # #             multiple = T, 
      # #             selectize = T, 
      # #             width = NULL, 
      # #             size = NULL),
      # # 
    ),
    mainPanel(
      plotOutput(outputId = "world_map")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  data("countries110")
  
  output$world_map <- renderPlot({
    ggplot() + 
      geom_sf(data = countries110[countries110$continent %in% input$f_continent,],
              aes(fill = continent), color = "black")+
      theme(panel.background = element_rect(fill = "#9ce4ff"), 
            legend.position = "none")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
