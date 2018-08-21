library(data.table)

setClass('ContrastB', contains = 'Contrasts')
setMethod("initialize",'ContrastB', function(.Object)
{
  .Object@ParameterNames = c('DiscountSums/favorable','AddictiveRate')
  .Object@SelectedCharacteristics = c('DiscountSums','FavorableSums',
                                      'balance','AddictiveRate')
  .Object@Header = new('Contrasts')@Header
  .Object
}
)


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



setMethod('setSelection','ContrastB', function(tObject, dataset, input)
{
  callNextMethod(tObject,dataset,input)
  print("That was that")
  candidate1 = subset(dataset,DiscountSums > input$variable1 & balance < input$variable2)
  candidate1$impervious = candidate1$DiscountSums * candidate1$balance
  candidate1$rn = rownames(candidate1)
  candidate2 = subset(dataset,FavorableSums > input$variable1 & balance < input$variable2)
  candidate2$impervious = candidate2$FavorableSums * candidate2$balance
  candidate2$rn = rownames(candidate2)
  candidate3 = subset(dataset,DiscountSums > input$variable1 & AddictiveRate < input$variable2)
  candidate3$impervious = candidate3$DiscountSums * candidate3$AddictiveRate
  candidate3$rn = rownames(candidate3)
  candidate4 = subset(dataset,FavorableSums > input$variable1 & AddictiveRate < input$variable2)
  candidate4$impervious = candidate4$FavorableSums * candidate4$AddictiveRate
  candidate4$rn = rownames(candidate4)
  tryCatch(
    {
      selected = rbind(candidate1,candidate2,candidate3, candidate4)
    }, error = {
      selected = dataset[0,]
    })
  if (dim(selected)[1] > 1)
  {
    selected = aggregate(selected,by=list(selected$rn), min)
    print("Assigning Names")
    tObject@Selected <- selected
    tObject@Websites <- selected$rn    
  } else {
    tObject@Selected <- selected
    tObject@Websites <- c('')
  }

  return(tObject)
})

setMethod("Visualize_Essence",'ContrastB', function(.Object, filter, lens){
  appel = lens
  print("Trying To Produce a Visualization...")
  
  if (dim(appel)[1] < 1 ){ return()}
  Contrasts2 = renderPlot({ 
    par(mfrow=c(2,2))
    twoord.plot(appel$sequence,appel$DiscountSums,
                appel$sequence,appel$balance, type='l', lcol = 'red', 
                rcol = 'green', ylab = 'DiscountSums', rylab = 'Balance',
                xlab = 'Time', xtickpos = appel$sequence, xticklab = appel$DATE) 
    twoord.plot(appel$sequence,appel$FavorableSums,
                appel$sequence,appel$balance, type='l', lcol = 'red', 
                rcol = 'green', ylab = 'FavorableSums', rylab = 'Balance',
                xlab = 'Time', xtickpos = appel$sequence, xticklab = appel$DATE)
    twoord.plot(appel$sequence,appel$FavorableSums,
                appel$sequence,appel$AddictiveRate, type='l', lcol = 'red', 
                rcol = 'green', ylab = 'FavorableSums', rylab = 'AddictiveRate',
                xlab = 'Time', xtickpos = appel$sequence, xticklab = appel$DATE)
    twoord.plot(appel$sequence,appel$DiscountSums,
                appel$sequence,appel$AddictiveRate, type='l', lcol = 'red', 
                rcol = 'green', ylab = 'DiscountSums', rylab = 'AddictiveRate',
                xlab = 'Time', xtickpos = appel$sequence, xticklab = appel$DATE)
    return(Contrasts2)
  })
})