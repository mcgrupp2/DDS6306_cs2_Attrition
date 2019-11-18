library(tidyverse)
library(shiny)
#install.packages("shinythemes")
library(shinythemes)
#install.packages("DT")
#library(DT)


#source("server.R")

#dataset <- read.csv("attritApp/data/CaseStudy2-data.csv")
#names(dataset)
#getwd()

dataset <- read.csv("data/CaseStudy2-data.csv")

fluidPage(

tagList(
  #shinythemes::themeSelector(),
  navbarPage(
    theme = shinytheme("yeti"),  # <--- To use a theme, uncomment this
    "shinythemes",
    tabPanel("Navbar 1",
             sidebarPanel(
               #fileInput("file", "File input:"),
               selectizeInput(
                 'e2', '2. Multi-select', choices = names(dataset), multiple = TRUE
               ),
               #textInput("txt", "Text input:", "general"),
               #sliderInput("slider", "Slider input:", 1, 100, 30),
               #tags$h5("Deafult actionButton:"),
               #actionButton("action", "Search"),
               
               #tags$h5("actionButton with CSS class:"),
               #actionButton("action2", "Action button", class = "btn-primary")
             ),
             mainPanel(
               tabsetPanel(
                 #~~~~~~~~This is the tabset panel 
                 tabPanel("Tab 1",
                          h4("Table"),
                          #tableOutput("table"),
                          h4("Verbatim text output"),
                          verbatimTextOutput("txtout"),
                          h1("Header 1"),
                          h2("Header 2"),
                          h3("Header 3"),
                          h4("Header 4"),
                          h5("Header 5")
                 ),
                 tabPanel("Tab 2", 
                          dataTableOutput("table")),
                 tabPanel("Tab 3", 
                          titlePanel("Basic DataTable"),
                          
                          # Create a new Row in the UI for selectInputs
                          fluidRow(
                            column(4,
                                   selectInput("man",
                                               "Manufacturer:",
                                               c("All",
                                                 unique(as.character(mpg$manufacturer))))
                            ),
                            column(4,
                                   selectInput("trans",
                                               "Transmission:",
                                               c("All",
                                                 unique(as.character(mpg$trans))))
                            ),
                            column(4,
                                   selectInput("cyl",
                                               "Cylinders:",
                                               c("All",
                                                 unique(as.character(mpg$cyl))))
                            )
                          ),
                          # Create a new row for the table.
                          dataTableOutput("table1"),
                          "Here's more")
               )
             )
    ),
    tabPanel("Navbar 2", names(df)),
    tabPanel("Navbar 3", "This panel is intentionally left blank")
  )
))
