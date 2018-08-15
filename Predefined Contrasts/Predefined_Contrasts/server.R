library(shiny)
library(plotrix)
source("helper_functions.R")

#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

Performance3MData = read.table("3months_performance",sep=';', header=TRUE)
RawData = read.table("raw_data", sep=';', header=TRUE)




###Identifying Problem Websites: step 1 manual, rulebase identification 3 months

## wagercounts >> and balance decreases
#ruleset1 = subset( fillzero,WagersCounts > 0 & balance < 0)
#ruleset1$impactfactor = ruleset1$balance * ruleset1$WagersCounts
#ordered_ruleset1 = ruleset1[order(ruleset1$impactfactor),]
#subselection = rownames( ordered_ruleset1[ ordered_ruleset1$impactfactor < -0.01,] )
#subset = Data[ Data$SystemCode %in% subselection,]



# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  observeEvent(input$Contrast,{
    pars = getParameterNames(input$Contrast)
    updateSliderInput(session,'variable1', label = pars[1])
    updateSliderInput(session,'variable2', label = pars[2])
    
    
    observeEvent(RawData,{                   
      updateSelectInput(session, 'Characteristics', choices = colnames(RawData))
    }
    )
    
    observeEvent(c(input$variable1,input$variable2),
                 {
                   selected = Performance3MData[0,]
                   selected$impervious = c()
                   if (input$Contrast == 'ContrastA')
                   {
                     selected = subset(Performance3MData,CommissionableSums > input$variable1 & balance < -input$variable2)
                     selected$impervious = selected$CommissionableSums * selected$balance
                   }
                   else 
                   {
                     return()
                   }
                   selected = selected[ order(selected$impervious), ]
                   websites = rownames(selected)
                   selectiondataset = RawData[RawData$SystemCode %in% websites,]
                   selectionglobalset = 
                   selectiondataset = selectiondataset[order(selectiondataset$SystemCode,selectiondataset$DATE),]
                   updateSelectInput(session, 'detailsWebsite', choices = websites)
                   observeEvent(c(input$detailsWebsite,input$Characteristics),
                                {
                                  tryCatch({
                                    backup = selectiondataset[ selectiondataset$SystemCode == input$detailsWebsite,input$Characteristics]
                                    appel = selectiondataset[ selectiondataset$SystemCode == input$detailsWebsite,]
                                    selectedsubset = selected[,colnames(selected) %in% input$Characteristics]
                                  }, error = {
                                    appel = selectiondataset[ selectiondataset$SystemCode == input$detailsWebsite,]
                                    backup = appel
                                    selectedsubset = selected
                                  } )
                                  output$summaryTable = DT::renderDataTable( backup )
                                  print(selectedsubset)
                                  print(intersect(unlist(input$Characteristics),  unlist(colnames(selected) )))
                                  print( colnames(selected))
                                  print(input$Characteristics)
                                  output$selectedDatasets = DT::renderDataTable( selectedsubset )
                                  if (input$Contrast == 'ContrastA')
                                  {
                                    output$Contrasts1 = renderPlot({ 
                                      twoord.plot(appel$sequence,appel$balance,
                                                  appel$sequence,appel$WagersCounts, type='l', lcol = 'red', 
                                                  rcol = 'green', ylab = 'Balance', rylab = 'CommissionableSums',
                                                  xlab = 'Time', xtickpos = appel$sequence, xticklab = appel$DATE) 
                                    })
                                  }
                                })
                 })
    

    

  })

})
