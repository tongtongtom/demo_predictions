library(shiny)
library(plotrix)
source("Contrasts.R")
source("ContrastA.R")
source("ContrastB.R")
source("ContrastC.R")
source("ContrastD.R")
source("ContrastE.R")
source("helper_functions.R")

Operator = NULL

#source("CountrastB.R")
#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


d7normreg = read.table( file = "../data/reg7dset" ,sep=';', stringsAsFactors = FALSE)
d7normdays = read.table( file = "../data/days7dset" ,sep=';',stringsAsFactors = FALSE)
d7standreg = read.table(file = "../data/reg7stand" ,sep=';', stringsAsFactors = FALSE)
d7standdays = read.table(file = "../data/days7stand" ,sep=';', stringsAsFactors = FALSE)

d30normreg = read.table( file = "../data/reg30dset" ,sep=';',stringsAsFactors = FALSE)
d30normdays = read.table( file = "../data/days30dset" ,sep=';',stringsAsFactors = FALSE)
d30standreg = read.table(file = "../data/reg30stand" ,sep=';',stringsAsFactors = FALSE)
d30standdays = read.table(file = "../data/days30stand" ,sep=';',stringsAsFactors = FALSE)

d90normreg = read.table( file = "../data/reg90dset" ,sep=';',stringsAsFactors = FALSE)
d90normdays = read.table( file = "../data/days90dset" ,sep=';',stringsAsFactors = FALSE)
d90standreg = read.table(file = "../data/reg90stand" ,sep=';',stringsAsFactors = FALSE)
d90standdays = read.table(file = "../data/days90stand" ,sep=';',stringsAsFactors = FALSE)

###Identifying Problem Websites: step 1 manual, rulebase identification 3 months

## wagercounts >> and balance decreases
#ruleset1 = subset( fillzero,WagersCounts > 0 & balance < 0)
#ruleset1$impactfactor = ruleset1$balance * ruleset1$WagersCounts
#ordered_ruleset1 = ruleset1[order(ruleset1$impactfactor),]
#subselection = rownames( ordered_ruleset1[ ordered_ruleset1$impactfactor < -0.01,] )
#subset = Data[ Data$SystemCode %in% subselection,]
RawData = d7normdays
relevant_dataset = d7normreg

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  datasets = reactiveValues()
  
  observeEvent(input$Dataset,{
    if( input$Dataset == '7days normalized')
    { 
      relevant_dataset <<- d7standreg 
      RawData <<- d7standdays
    } else if ( input$Dataset == '7 days')
    {
      relevant_dataset <<- d7normreg 
      RawData <<- d7normdays
    } else if( input$Dataset == '30 days normalized')
    {
      relevant_dataset <<- d30standreg 
      RawData <<- d30standdays
    } else if (input$Dataset == '30 days')
    {
      relevant_dataset <<- d30normreg 
      RawData <<- d30normdays
    } else if( input$Dataset == '90 days normalized')
    {
      relevant_dataset <<- d90standreg 
      RawData <<- d90standdays
    } else if (input$Dataset == '90 days')
    {
      relevant_dataset <<- d90normreg 
      RawData <<- d90normdays
    }
    datasets$relevant_dataset = relevant_dataset
    datasets$RawData          = RawData
  })
  
  observeEvent(c(input$CompareVariable1,input$CompareVariable2),
  {
    if (is(Operator,'Contrasts'))
    {  
    Operator <<- SetParameters(Operator, c(input$CompareVariable1,input$CompareVariable2))
    updateSliderInput(session,'variable1', label = input$CompareVariable1)
    updateSliderInput(session,'variable2', label = input$CompareVariable2)
    updateSelectInput(session, 'Characteristics', choices = colnames(RawData), 
                      selected = c(input$CompareVariable1,input$CompareVariable2))
    }
 })
  
  observeEvent(c(input$Contrast,datasets),{
    Operator <<- new(input$Contrast)
    updateSelectInput(session,'CompareVariable1', choices = colnames(getSelection(Operator)))
    updateSelectInput(session,'CompareVariable2', choices = c('None', colnames(getSelection(Operator))))    
    pars = getParameterName(Operator)
    basis = getSelectedCharacteristics(Operator)
    ##Can be moved into the class
    updateSliderInput(session,'variable1', label = pars[1])
    updateSliderInput(session,'variable2', label = pars[2])
    
    observeEvent(RawData,{                   
      updateSelectInput(session, 'Characteristics', choices = colnames(RawData), selected = basis)
    }
    )
    
    observeEvent(c(input$variable1,input$variable2, datasets),
                 {
                   relevant_dataset =  datasets$relevant_dataset
                   RawData =  datasets$RawData
                   pars = getParameterName(Operator)
                   basis = getSelectedCharacteristics(Operator)

                   Operator = setSelection(Operator, relevant_dataset, input)
                   selected = getSelection(Operator)
                   tryCatch({ selected = selected[order(selected$impervious,decreasing=T),]},except={})
                   Websites = getSites(Operator)
                   filtered = getFiltered(Operator,  RawData)

                  updateSelectInput(session, 'detailsWebsite', choices = Websites, selected=
                                       selected$SystemCode[1])
                   observeEvent(c(input$detailsWebsite,input$Characteristics),
                                {
                                  BasicDataSet = filtered[ filtered$SystemCode ==  input$detailsWebsite,]
                                  tryCatch({
                                    DataSetColumns    = BasicDataSet[,colnames(BasicDataSet) %in% c(input$Characteristics,
                                                                        'SystemCode','DATE')]
                                    HighlightedSubset = selected[ ,colnames(selected) %in% c(input$Characteristics,
                                                                        'SystemCode','DATE','impervious')]
                                  }, error = {
                                    DataSetColumns   = BasicDataSet
                                    HighlightedSubset = selected
                                  } )
                                  
                                  tryCatch({
                                    output$summaryTable = DT::renderDataTable(DataSetColumns)
                                  }, error = {
                                    output$summaryTable = NULL
                                  })
                                  tryCatch({
                                    output$selectedDatasets = DT::renderDataTable( HighlightedSubset )
                                  }, error = {
                                    output$selectedDatasets = NULL
                                  })
                                  tryCatch({
                                    output$Contrasts1 = Visualize_Essence(Operator, input$detailsWebsite, BasicDataSet)  
                                  }, error = {
                                    output$Contrasts1 = NULL
                                  })
                                

                 })
    

    
                 })
  })
  
})
