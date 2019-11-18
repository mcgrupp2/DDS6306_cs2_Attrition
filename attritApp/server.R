library(shiny)
library(tidyverse)

function(input, output, session) {
  
  dataset <- reactive({
    read.csv("data/CaseStudy2-data.csv")
  })
  
  output$table <- DT::renderDataTable(DT::datatable(data = dataset()))
  
  #output$table <- renderDataTable(datatable({
  #  data <- mpg
  #  if (input$man != "All") {
  #    data <- data[data$manufacturer == input$man,]
  #  }
  #  if (input$cyl != "All") {
  #    data <- data[data$cyl == input$cyl,]
  #  }
  #  if (input$trans != "All") {
  #    data <- data[data$trans == input$trans,]
  #  }
  #  data
  #}))
  
  
  
  output$txtout <- renderText({
    paste(input$txt, input$slider, format(input$date), sep = ", ")
  })
  output$table1 <- renderTable({
    head(cars, 4)
  })
}