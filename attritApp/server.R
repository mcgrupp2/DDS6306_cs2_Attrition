library(shiny)
library(tidyverse)

function(input, output, session) {
  
  dataset <- reactive({
    read.csv("attritApp/data/CaseStudy2-data.csv")
  })
  
  
  
  output$txtout <- renderText({
    paste(input$txt, input$slider, format(input$date), sep = ", ")
  })
  output$table <- renderTable({
    head(cars, 4)
  })
}