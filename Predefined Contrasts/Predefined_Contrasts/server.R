library(shiny)
library(plotrix)
library(robustbase)
library(shinyRGL)
library(rgl)

source("Contrasts.R")
source("ContrastA.R")
source("ContrastB.R")
source("ContrastC.R")
source("ContrastD.R")
source("ContrastE.R")
source("ContrastZZ.R")
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
  
  observeEvent(c(input$Dataset,input$MinUsers, input$AvgUsers),{
    if( input$Dataset == '7days normalized')
    { 
      indxs = filtermin(d7standdays, input$MinUsers, input$AvgUsers)
      relevant_dataset <<- d7standreg[indxs, ] 
      RawData <<- d7standdays[d7standdays$SystemCode %in% indxs, ]
    } else if ( input$Dataset == '7 days')
    {
     indxs = filtermin(d7normdays, input$MinUsers, input$AvgUsers)
      relevant_dataset <<- d7normreg[indxs,] 
      RawData <<- d7normdays[ d7normdays$SystemCode %in% indxs, ]
    } else if( input$Dataset == '30 days normalized')
    {
      indxs = filtermin(d30standdays, input$MinUsers, input$AvgUsers)
      relevant_dataset <<- d30standreg[indxs,] 
      RawData <<- d30standdays[ d30standdays$SystemCode %in% indxs,]
    } else if (input$Dataset == '30 days')
    {
      indxs = filtermin(d30normdays, input$MinUsers, input$AvgUsers)
      relevant_dataset <<- d30normreg[indxs,] 
      RawData <<- d30normdays[ d30normdays$SystemCode %in% indxs,]
    } else if( input$Dataset == '90 days normalized')
    {
      indxs = filtermin(d90standdays, input$MinUsers, input$AvgUsers)
      relevant_dataset <<- d90standreg[indxs,] 
      RawData <<- d90standdays[ d90standdays$SystemCode %in% indxs,]
    } else if (input$Dataset == '90 days')
    {
      indxs = filtermin(d90normdays, input$MinUsers, input$AvgUsers)
      relevant_dataset <<- d90normreg[indxs,] 
      RawData <<- d90normdays[ d90normdays$SystemCode %in% indxs,]
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
    
    
    filterout = function(ip){
      return(ip[!(ip %in% c('SystemCode','DATE','sequence')) ])
    }
    
    
    observeEvent(c(datasets,input$Characteristics),{
      ###Create 3D PCA representation
      if (input$overlays ==3)
      {
          print("OK, tot hier")
          filt_chars = filterout(input$Characteristics)
          cols_ds = colnames(datasets$relevant_dataset) %in% filt_chars
          cols_RD = colnames(datasets$RawData) %in% filt_chars
          tempdata = datasets$relevant_dataset
          if (dim(tempdata)[2] > 2)
          {
          filtered =tempdata[,cols_ds]
          
          tryCatch({
          if ((sum(cols_ds) > 2)){
          model1 =PcaHubert(x = tempdata[,cols_ds], k= 50)
          md1 = getScores(model1)[,c(1:3)]
          Mean = getCenter(CovMve(md1))
          Sigma = getCov(CovMve(md1))
          
          #output$myWebGL = renderRglwidget({
          #  n <- 10
          #  try(rgl.close())
          #  plot3d(rnorm(n), rnorm(n), rnorm(n))
          #  rglwidget()
          #})
          
          output$PCA_plot = renderRglwidget({
            try(rgl.close())
            plot3d(ellipse3d(Sigma,centre=Mean,level=0.99999), col = "blue", alpha = 0.5, aspect = TRUE,shape='points')
            points3d(md1, col='red', shape='points')
            rglwidget()
          })
          
          
          ###Now the more difficult part:
          
          }
          },except = {
            
          })
          }
       
      }
    })
    
    observeEvent(c(input$Dataset,input$Characteristics, input$topn),
                  {
                    if (input$overlays == 2){
                      filt_chars = filterout(input$Characteristics)
                      cols_ds = colnames(datasets$relevant_dataset) %in% filt_chars
                      cols_RD = colnames(datasets$RawData) %in% filt_chars
                      selected_MT = as.data.frame(datasets$relevant_dataset[,cols_ds])
                      basis_MT    = as.data.frame(datasets$RawData[,])
                      tryCatch({
                      if ((dim(selected_MT)[1] > 1) && (dim(selected_MT)[2] > 1))
                        {
                        x = robustbase::covMcd(selected_MT)
                        top_n_rows = (rownames(selected_MT)[ order(x$mah, decreasing =TRUE) ])[1:input$topn]
                        output$comparisonList =
                        renderPlot({
                          matplot(t(rbind(atan(x$center),atan(selected_MT[top_n_rows,]))),type='l', xlab = 'Category', ylab ='evolution',
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
                            datainput = eval(parse(text = paste('xtabs(formula =',eval(rowlov),
                                            '~sequence + SystemCode, data = tmporset)',sep='')))
                            output[[plotname]] <- renderPlot({
                              matplot(datainput,
                                      main = rowlov, type='l'
                              )
                              legend("bottomright", inset=.05, legend=top_n_rows, pch=1, horiz=FALSE,
                                     col = c(1:length(top_n_rows)))
                              
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
