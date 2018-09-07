#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
options(stringsAsFactors=FALSE)
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
  updateSelectInput(session,'Char1',choices= filterout(colnames(Datasets[['RawData']])))
  updateSelectInput(session,'Char2',choices= filterout(colnames(Datasets[['RawData']])))
  
   observeEvent(c(input$Dataset,input$MinUsers, input$AvgUsers),{
     Datasets <<- getDataSet(input$Dataset,input$MinUsers,input$AvgUsers, 'regression',datasets)
     updateSelectInput(session,'detailsWebsite',  choices = unique((Datasets[['relevant_data']])$SystemCode))
   })
   
   observeEvent(c(input$AllSites,input$AllCharacteristics),
                {
                  if (input$AllSites)
                  {    
                    updateSelectInput(session,'detailsWebsite',  selected = unique((Datasets[['relevant_data']])$SystemCode))
                  }
                  if (input$AllCharacteristics)
                  {
                    updateSelectInput(session,'Characteristics',selected = filterout(colnames(Datasets[['RawData']])))
                  }
                })
  
   output$General_Correlation <- renderPlot(width=1200, height=1000,{heatmap(correlation_matrix)})
  
   observeEvent(input$updateValues,{
     if(input$overlays==2)
     {
       datasets = Datasets[['RawData']]
       
       counter = 1
       number_of_sets = length(unique(datasets$SystemCode))
       dat = get_n(number_of_sets)
       print(dat)
       
       pair_corr = cor(datasets[,c(input$Char1,input$Char2)])[2,1]
       basisset = c('gen',pair_corr)
       
       for (sequence in unique(data$SystemCode))
       {
         zm = cor(datasets[data$SystemCode == sequence,c(input$Char1,input$Char2)])[2,1]
         basisset = rbind(basisset,c(sequence,zm))
       }
       basisset[is.na(basisset)] = 0.0
       colnames(basisset) = c("Website",'Correlation')
       output$PairwiseCorrelationTable = DT::renderDataTable({basisset})
       
       
       output$CorrelationTableau = renderPlot({
         tegenstroom = datasets[,c(input$Char1,input$Char2,'SystemCode','sequence')]
         par(mfrow=c(dat[1],dat[2]))
         par(mar=c(1,1,1,1))
       
         for (id in unique(tegenstroom$SystemCode))
        {
          tempdat = tegenstroom[ tegenstroom$SystemCode == id, ]
          tmpval = cor(tempdat[,c(input$Char1,input$Char2)])[1,2]
          tmpval[is.na(tmpval)] = 0
          if(abs( tmpval - pair_corr) > 1){
            plot(tempdat[,c(input$Char1,input$Char2)],col='yellow')
            points(lowess(tempdat[,c(1:2)]),type='l',col='red')
         } else {
            plot(tempdat[,c(input$Char1,input$Char2)],col='green')
            points(lowess(tempdat[,c(1:2)]),type='l',col='black')
         }
       }
       
       })
     }
   })
   
   observeEvent(input$updateValues,{
     if(input$overlays==4)
     {
       datasets = Datasets[['RawData']]
       parameters = filterout(colnames(datasets))
       print(parameters)
       xmm = t(data.frame(c('par1','par2',0)))
       for (outer in c(1:length(parameters))){
         for (inner in c(1:(outer-1) ))
         {
           sel1 = parameters[outer]
           if((outer-inner) < 1){next}
           if (inner == 0){next}
            sel2 = parameters[inner]
            print(paste(sel1,sel2,sep=' '))
           tpvalue = cor(datasets[,c(sel1,sel2)])[2,1]
           xmm = rbind(xmm,c(sel1,sel2,tpvalue))
           
           for (summer in unique(datasets$SystemCode))
           {
             reldat = datasets[datasets$SystemCode ==summer,]
             if ((mean(reldat[,sel1]== 0)|| mean(reldat[,sel2]==0))){
               xmm = rbind(xmm,c(sel1,sel2,0))
               next
             }
             tpval2 = cor(datasets[datasets$SystemCode == summer,c(sel1,sel2)])[2,1]
             xmm = rbind(xmm,c(sel1,sel2,tpval2))
           }
           print(head(xmm))
         }
       }
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
         dam = c('',
                 cor.test(as.vector(Comparison),as.vector(baseline))$estimate,
                 cor.test(as.vector(Comparison),as.vector(baseline))$p.value
                 )
         Comparisons = rbind(Comparisons,unlist(dam))
       }
       Comparisons$Comparison = c('Comparison', unique(selected$SystemCode))
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
