library(shiny)
library(plotrix)
library(robustbase)
library(shinyRGL)
library(rgl)
library(rrcov)

source("Contrasts.R")
source("ContrastA.R")
source("ContrastB.R")
source("ContrastC.R")
source("ContrastD.R")
source("ContrastE.R")
source("ContrastZZ.R")
source("helper_functions.R")
source("DataInput.R")

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


datasets = readindata()
###Identifying Problem Websites: step 1 manual, rulebase identification 3 months

## wagercounts >> and balance decreases
#ruleset1 = subset( fillzero,WagersCounts > 0 & balance < 0)
#ruleset1$impactfactor = ruleset1$balance * ruleset1$WagersCounts
#ordered_ruleset1 = ruleset1[order(ruleset1$impactfactor),]
#subselection = rownames( ordered_ruleset1[ ordered_ruleset1$impactfactor < -0.01,] )
#subset = Data[ Data$SystemCode %in% subselection,]
RawData = datasets[['d7normdays']]
relevant_Dataset = datasets[['d7normreg']]

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  Datasets = reactiveValues()
  
  observeEvent(c(input$Dataset,input$MinUsers, input$AvgUsers, input$Aggregation),{
    Datasets <<- getDataSet(input$Dataset,input$MinUsers,input$AvgUsers, input$Aggregation,datasets)
    updateSelectInput(session,'summary_dataset',  choices = unique((Datasets[['RawData']])$SystemCode))
  })

  observeEvent(c(input$AllCharacteristics,input$AllSites),{
    if (input$AllCharacteristics){
      updateSelectInput(session, 'Characteristics', selected = colnames(Datasets[['RawData']]))
    }
    if (input$AllSites){
      updateSelectInput(session,'summary_dataset',  selected = unique((Datasets[['RawData']])$SystemCode))
    }
  })
    
  observeEvent(c(input$CompareVariable1,input$CompareVariable2),
  {
    if (is(Operator,'Contrasts'))
    {  
    Operator <<- SetParameters(Operator, c(input$CompareVariable1,input$CompareVariable2))
    updateSliderInput(session,'variable1', label = input$CompareVariable1)
    updateSliderInput(session,'variable2', label = input$CompareVariable2)
    updateSelectInput(session, 'Characteristics', choices = colnames(Datasets[['RawData']]), 
                      selected = c(input$CompareVariable1,input$CompareVariable2))
    }
 })
  
  observeEvent(c(input$Contrast,Datasets),{
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
    
    

    
    observeEvent(input$updateButton,{
      ###Create 3D PCA representation
      if (input$overlays ==3)
      {
          print("OK, tot hier")
          filt_chars = filterout(input$Characteristics)
          cols_ds = colnames(Datasets$relevant_Dataset) %in% filt_chars
          cols_RD = colnames(Datasets$RawData) %in% c(filt_chars,'sequence','SystemCode')
          tempData = Datasets$relevant_Dataset
          filtdata = Datasets$RawData
          if (dim(tempData)[2] > 2)
          {
          filtered =tempData[,cols_ds]
          
          tryCatch({
          if ((sum(cols_ds) > 2)){
          model1 =PcaHubert(x = tempData[,cols_ds], k= 50)
          md1 = getScores(model1)[,c(1:3)]
          Mean = getCenter(CovMve(md1))
          Sigma = getCov(CovMve(md1))
          
          stretcher = estimate_probability(input$pcascale)
          statvalues  = as.data.frame(diag(((md1[,]-Mean) %*% MASS::ginv(Sigma)) %*% t(md1-Mean)))
          outliers = (rownames(statvalues)[ sqrt(statvalues[,1]) > stretcher])
          
          relic = filtdata[filtdata$SystemCode %in% outliers,cols_RD]
          subsetter = colnames(relic) %in% filt_chars
          
          create_summary(relic[subsetter], list(relic$SystemCode))

          output$PCA_summary = DT::renderDataTable(filtdata[filtdata$SystemCode %in% outliers,cols_RD])
          
          ###Now the more difficult part: isolating the outliers
          output$PCA_plot = renderRglwidget({
            try(rgl.close())
            plot3d(ellipse3d(Sigma,centre=Mean,scale = c(stretcher,stretcher,stretcher), level = 0.2), col = "blue", alpha = 0.5, aspect = TRUE,shape='points')
            points3d(md1[rownames(md1) %in% outliers, ],col='black', shape='points')
           
            points3d(md1, col='red', shape='points')
            rglwidget()
          })
          
          

          
          }
          },except = {
            
          })
          }
       
      }
    })
    
    observeEvent(input$updateButton,
                  {
                    if (input$overlays == 2){
                      filt_chars = filterout(input$Characteristics)
                      cols_ds = colnames(Datasets$relevant_Dataset) %in% filt_chars
                      cols_RD = colnames(Datasets$RawData) %in% filt_chars
                      selected_MT = Datasets$relevant_Dataset[,cols_ds]
                      basis_MT    = Datasets$RawData[,]
                      tryCatch({
                      if(sum(cols_ds) < 2) { return()}
                      if ((dim(selected_MT)[1] > 1) && (dim(selected_MT)[2] > 1))
                        {
                        x = rrcov::CovMve(selected_MT)
                        
                        top_n_rows = (rownames(selected_MT)[ order(x@raw.mah, decreasing =TRUE) ])[1:input$topn]
                        print("Top n rows:")
                        print(top_n_rows)
                        print("CenterValues:")
                        std = atan(rbind(x@raw.center, selected_MT[top_n_rows,]))
                        #print(std)
                        #print(sweep(t(std),1,apply(std,2,function(x) max(abs(x))),'/'))
                        std = t(t(std) / apply(std,2,function(x) max(abs(x))))
                        output$comparisonList =
                        renderPlot({
                          matplot(t(std),type='l', xlab = 'Category', ylab ='evolution',
                                axes =F, col = c(1:(input$topn +1)))
                        legend("bottomright", inset=.05, legend=c('mean', top_n_rows), pch=1, horiz=FALSE,
                               col = c(1:(input$topn+1)))
                        axis(2)
                        axis(side=1, at=1:length(filt_chars), labels = filt_chars, outer= FALSE)
                        })
                        output$Comparison = DT::renderDataTable(selected_MT)
                        
                        output$DistanceDistance = renderPlot({plot(x,which='dd')})
                        # Insert the right number of plot output objects into the web page
                        output$ComparedRawData <- renderUI({
                          plot_output_list <- lapply(1:length(filt_chars), function(i) {
                            plotname <- paste("Harbour", i, sep="")
                            plotOutput(plotname, height = 500, width = 750)
                          })
                          
                          # Convert the list to a tagList - this is necessary for the list of items
                          # to display properly.
                          do.call(tagList, plot_output_list)
                        })
                        
                        for (i in 1:length(filt_chars)) {
                          # Need local so that each item gets its own number. Without it, the value
                          # of i in the renderPlot() will be the same across all instances, because
                          # of when the expression is evaluated.
                          local({
                            my_i <- i
                            plotname <- paste("Harbour", my_i, sep="")
                            rowlov= filt_chars[my_i]
                            tmporset = subset(basis_MT[,c('sequence','SystemCode',rowlov)], 
                                                SystemCode %in%  top_n_rows)
                            Datainput = eval(parse(text = paste('xtabs(formula =',eval(rowlov),
                                            '~sequence + SystemCode, data = tmporset)',sep='')))
                            output[[plotname]] <- renderPlot({
                              matplot(Datainput,
                                      main = rowlov, type='l', col = c(2:(length(top_n_rows)+1))
                              )
                              legend("bottomright", inset=.05, legend=top_n_rows, pch=1, horiz=FALSE,
                                     col = c(2:(length(top_n_rows)+1)))
                              
                            })
                          })
                        }
                      } else { 
                          output$comparisonList   = renderPlot({})
                          output$Comparison       = DT::renderDataTable(data.frame())
                          output$DistanceDistance = renderPlot({})
                          output$ComparedRawData  = renderUI({})
                        }
                      }
                      , except={})
                      
                    }
                  })
    
    
    observeEvent(input$updateButton,
                 {
                   relevant_Dataset =  Datasets$relevant_Dataset
                   RawData =  Datasets$RawData
                   pars = getParameterName(Operator)
                   basis = getSelectedCharacteristics(Operator)

                   Operator = setSelection(Operator, relevant_Dataset, input)
                   selected = getSelection(Operator)
                   tryCatch({ selected = selected[order(selected$impervious,decreasing=T),]},except={})
                   Websites = getSites(Operator)
                   filtered = getFiltered(Operator,  RawData)

                   #print(Websites)
                   #print(head(filtered))
                   #print(head(selected))
                   
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
  
  observeEvent(input$updateButton,{
    if (input$overlays == 4)
    {
      
    }
  })
  
})
