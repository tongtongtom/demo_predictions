#library(data.table)
#
setClass('ContrastZZ', contains = 'Contrasts')
setMethod("initialize",'ContrastE', function(.Object)
{
  .Object@ParameterNames =c('Parameter1','Parameter2')
  .Object@SelectedCharacteristics = c('Parameter1','Parameter2')
  .Object@Header = new('Contrasts')@Header
  .Object@Selected = new('Contrasts')@Selected
  .Object@Websites = ''
}
)


setMethod('setSelection','ContrastZZ', function(tObject, dataset, input)
{
  callNextMethod(tObject,dataset,input)

  parameter1 = input$CompareVariable1
  
  print(input$CompareVariable1)
  print(input$CompareVariable2)
  
  if (input$CompareVariable1 == '') { return(tObject)}
  candidate1 = data.frame()
  
  if (input$CompareVariable2 != 'None')
  { 
    parameter2 = input$CompareVariable2
    candidate1 = dataset[ (dataset[,eval(parameter1)] > input$variable1) & (dataset[,eval(parameter2)] < input$variable2),]
    if (dim(candidate1)[1] >= 1) {
        candidate1$impervious = candidate1[,parameter1] * candidate1[,parameter2]
    } else {
      return(tObject)
    }
  } else {
    candidate1 = dataset[ (dataset[,eval(parameter1)] > input$variable1),]
    if (dim(candidate1)[1] >= 1 )
    {
    candidate1$impervious = candidate1[,parameter1] 
    } else {
      return(tObject)
    }
  }
    tObject@Selected <- candidate1
    tObject@Websites <- rownames(candidate1)
    return(tObject)
})

setMethod("Visualize_Essence",'ContrastZZ', function(.Object, filter, lens){
  appel = lens
  print("Trying To Produce a Visualization...")
  params = .Object@SelectedCharacteristics
  if (dim(appel)[1] < 1 ){ return()}

  if (params[2] != 'None')
    {
    Contrasts2 = renderPlot({ 
      twoord.plot(appel[,'sequence'],appel[,params[1]],appel[,'sequence'],appel[,params[2]],type='l', lcol = 'red', 
                rcol = 'green', ylab = params[1], rylab = params[2],
                xlab = 'Time') #, xticklab = appel$DATE, xtickpos = appel$sequence
    })
  } else {
    Contrasts2 = renderPlot({ 
      plot(appel[,params[1]],appel[,'sequence'], type='l', col = 'red', 
                  ylab = params[1], xlab = 'Time') 
    })
  }
})


#selected = subset(Performance3MData,CommissionableSums > input$variable1 & balance < -input$variable2)
#selected$impervious = selected$CommissionableSums * selected$balance
#candidate1 = subset(Performance3MData,DiscountSums > input$variable1 & balance < -input$variable2)
#candidate1$impervious = candidate1$DiscountSums * candidate1$balance
#candidate1$rn = rownames(candidate1)
#candidate2 = subset(Performance3MData,FavorableSums > input$variable1 & balance < -input$variable2)
#candidate2$impervious = candidate2$FavorableSums * candidate2$balance
#candidate2$rn = rownames(candidate2)
#candidate3 = subset(Performance3MData,DiscountSums > input$variable1 & AddictiveRate < -input$variable2)
#candidate3$impervious = candidate3$DiscountSums * candidate3$AddictiveRate
#candidate3$rn = rownames(candidate3)
#candidate4 = subset(Performance3MData,FavorableSums > input$variable1 & AddictiveRate < -input$variable2)
#candidate4$impervious = candidate4$FavorableSums * candidate4$AddictiveRate
#candidate4$rn = rownames(candidate4)
#selected = rbind(candidate1,candidate2,candidate3, candidate4)
#selected = aggregate(selected,by=list(selected$rn), min)
#rownames(selected) = selected$rn
#return(selected)


      
