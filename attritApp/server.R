library(shiny)
library(GGally)
library(tidyverse)

df <- read.csv("data/CaseStudy2-data.csv")

function(input, output, session) {
  
  dataset <- reactive({
    
    if (is.null(input$colIns)) {
      df
    }else{
      
      df[,input$colIns]
    }
  })
  
  output$table <- DT::renderDataTable(DT::datatable(data = dataset()))
  
  #output$colnames <- reactive({input$colIns})
  
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
  
  selectedData <- reactive({
    df[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  output$plot2 <- renderPlot({
    p <- ggpairs(dataset(), ggplot2::aes(colour=dataset()$Attrition))
    GGally::print_if_interactive(p)
  })
  
  
  output$txtout <- renderText({
    paste(input$txt, input$slider, format(input$date), sep = ", ")
  })
  output$table1 <- renderTable({
    head(cars, 4)
  })
}