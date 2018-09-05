#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
source('HelperFunctions.R')

library(shiny)
library(DT)
library(reshape)

datasets = readindata()
Datasets = list()
Datasets[['RawData']] = datasets[['d7normdays']]
Datasets[['relevant_data']] = datasets[['d7normreg']]


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  cols = filterout(colnames(Datasets[['RawData']]))
  correlation_matrix = cor(Datasets[['RawData']][,cols])
  
  output$General_Correlation_table <- {
    DT::renderDataTable(melt(correlation_matrix))
  }
   
  updateSelectInput(session,'Characteristics',choices= filterout(colnames(Datasets[['RawData']])))
  updateSelectInput(session,'detailsWebsite',  choices = unique((Datasets[['relevant_data']])$SystemCode))
  
   observeEvent(c(input$Dataset,input$MinUsers, input$AvgUsers),{
     Datasets <<- getDataSet(input$Dataset,input$MinUsers,input$AvgUsers, 'regression',datasets)
     updateSelectInput(session,'detailsWebsite',  choices = unique((Datasets[['relevant_data']])$SystemCode))
   })
   
   observeEvent(c(input$AllSites,input$AllCharacteristics),
                {
                  print(input$AllSites)
                  print(input$AllCharacteristics)
                  if (input$AllSites)
                  {    
                    updateSelectInput(session,'detailsWebsite',  selected = unique((Datasets[['relevant_data']])$SystemCode))
                    #input$AllSites = 0
                  }
                  if (input$AllCharacteristics)
                  {
                    updateSelectInput(session,'Characteristics',selected = filterout(colnames(Datasets[['RawData']])))
                    #input$AllCharacteristics = 0
                  }
                })
  
   output$General_Correlation <- renderPlot(width=1200, height=1000,{heatmap(correlation_matrix)})
  
   observeEvent(input$updateValues,{
     if(input$overlays==2)
     {
       
     }
   })
   
   observeEvent(input$updateValues,{
     if(input$overlays==3)
     {
       selected = Datasets[['RawData']]
       selected = selected[ selected$SystemCode %in% input$detailsWebsite, 
                            c('SystemCode',input$Characteristics)]
       baseline = correlation_matrix[input$Characteristics, input$Characteristics]
       baseline[is.na(baseline)] = 0.0
       
       Comparisons= data.frame('General',1,0)
       colnames(Comparisons) = c('Comparison','Similarity','p-value')
       for (systemcode in unique(selected$SystemCode)){
         Comparison = cor(selected[ selected$SystemCode == systemcode,
                          input$Characteristics])
         Comparison[is.na(Comparison)] = 0.0
         dam = c(systemcode,
                 cor.test(as.vector(Comparison),as.vector(baseline))$estimate,
                 cor.test(as.vector(Comparison),as.vector(baseline))$p.value
                 )
         print(dam)
         Comparisons = rbind(Comparisons,unlist(dam))
       }
       output$ComparisonTable = DT::renderDataTable({Comparisons})
     }
   })
   
   observeEvent(input$updateValues,{
     if ( (length(input$Characteristics) >= 2) & (input$overlays==1))
     {
      output$General_Correlation <- renderPlot(width=1200, height=1000,
      {
        heatmap(
        correlation_matrix[input$Characteristics, input$Characteristics])
      })
      selected = Datasets[['RawData']]
      selected = selected[ selected$SystemCode %in% input$detailsWebsite, input$Characteristics]
      sel_corr_matrix = cor(selected)
      output$Selected_Correlation <- renderPlot(width=1200,height=1000,
                                     {
                                       heatmap(
                                         sel_corr_matrix
                                       )
                                     })

     }
   })
  
})
