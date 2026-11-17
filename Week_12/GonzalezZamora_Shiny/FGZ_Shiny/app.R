#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(shinyWidgets)

# Run the application 

ui <- fluidPage(
  sliderInput(inputId = "num", # ID name for the input
              label = "Choose a number", # Label above the input
              value = 25, min = 1, max = 100 # values for the slider
  ),
  textInput(inputId = "title", # new Id is title
            label = "Write a title",  # starting title
            value = "Histogram of Random Normal Values"),
  sidebarLayout(
    sidebarPanel =
      selectInput(
        inputId = "col",
        label = "Select a Color",
        choices = c(
          "Steel Blue" = "steelblue", 
          "Corn Flower Blue" = "cornflowerblue", 
          "Brick Red" = "firebrick", 
          "Forest Green" = "forestgreen")
  ),
  mainPanel(
  plotOutput("hist"), # creates space for a plot called hist
  verbatimTextOutput("stats") # create a space for stats
)))

server <- function(input, output){
  data <- reactive({
    tibble(x = rnorm(input$num)) # number of points set by the slider
  })
  
  output$hist <- renderPlot({
    ggplot(data(), 
           aes(x = x)) + # make a histogram
      geom_histogram(
        fill = input$col,
        color = "white") +
      labs(title = input$title) # add a new title
  })
  output$stats <- renderPrint({
    summary(data()) # calculate summary stats based on the numbers
  })
}
shinyApp(ui = ui, server = server)
