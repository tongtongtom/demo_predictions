#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("Correlation Data"),

  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "Dataset", multiple = FALSE, label ='dataset', 
                  choices = list('7days normalized','7 days','30 days normalized','30 days',
                                 '90 days normalized','90 days'),
                  selected = '7 days'),
      sliderInput(inputId = 'MinUsers', label = 'Minimum User', min = 0, max = 50000, value  = 50, step = 1),
      sliderInput(inputId = 'AvgUsers', label = 'Average User', min = 0, max = 50000, value  = 0, step = 1),
      conditionalPanel(condition='input.overlays==1 | input.overlays==3',
      selectInput(inputId = "Characteristics", multiple=TRUE, label="Characteristics:", choices = list()),
      selectInput(inputId = "detailsWebsite", multiple=TRUE, label ="Details Website",  choices = list())),
      
      actionButton(inputId = 'AllSites', label= 'All Websites' ),
      actionButton(inputId = 'AllCharacteristics', label= 'All Characteristics' ),
      actionButton(inputId = "updateValues", label='update'),
      conditionalPanel(condition='input.overlays==2',
      selectInput(inputId = 'Char1', label='Characteristic 1',choices=list()),                 
      selectInput(inputId = 'Char2', label='Characteristic 2',choices=list()))
    ),
    mainPanel(
      tabsetPanel(id='overlays',
        tabPanel('General Correlation:',
                 plotOutput("General_Correlation" ,width = '1200px',  height='1500px'),
                 plotOutput("Selected_Correlation",width = '1200px', height='1500px'),
                 DT::dataTableOutput('General_Correlation_table'),
                 value = 1
                 ),
        tabPanel("Pairwise Correlations",
                 DT::dataTableOutput("PairwiseCorrelationTable"),
                 textOutput("A table of Correlations"),
                 plotOutput("CorrelationTableau",width='1200px',height='1500px'),
                 textOutput("First parameter vs Correlation"),
                 plotOutput("CorrelationFirstTime",width='1200px', height='1500px'),
                 textOutput("Second parameter vs Correlation"),
                 plotOutput("CorrelationSecondTime",width='1200px', height='1500px'),
                 value=2),
        tabPanel("Anti-Correlated Websites",
                 value=4),
        tabPanel("Distance in selected Correlations",
                 DT::dataTableOutput("ComparisonTable"),
                 value=3)
        )
    )
  )
  ))
