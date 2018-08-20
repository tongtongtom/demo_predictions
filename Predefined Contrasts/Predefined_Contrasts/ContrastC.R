setClass('ContrastC', contains = 'Contrasts')
setMethod("initialize",'ContrastC', function(.Object)
{
  .Object@ParameterNames = c('AddictiveRate','FavorableSums')
  .Object@SelectedCharacteristics = c('AddictiveRate','DiscountSums',
                                      'FavorableSums')
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



setMethod('setSelection','ContrastC', function(tObject, dataset, input)
{
  callNextMethod(tObject,dataset,input)
  print("That was that")
  candidate1 = subset(dataset,AddictiveRate > input$variable1 & DiscountSums < input$variable2)
  candidate1$impervious = candidate1$DiscountSums * candidate1$AddictiveRate
  candidate1$rn = rownames(candidate1)
  candidate2 = subset(dataset,AddictiveRate > input$variable1 & FavorableSums < input$variable2)
  candidate2$impervious = candidate2$FavorableSums * candidate2$AddictiveRate
  candidate2$rn = rownames(candidate2)
  tryCatch(
    {
      selected = rbind(as.data.frame(candidate1),as.data.frame(candidate2))
      if (dim(selected)[1] > 1){
        selected = aggregate(selected,by=list(selected$rn), min)
        tObject@Selected <- selected
        tObject@Websites <- selected$rn
      } else {
        tObject@Selected <- dataset[0,]
        tObject@Websites <- c('')
      }
    }, error = {
      tObject@Selected <- dataset[0,]
      tObject@Websites <- c('')
    })
  print("Assigning Names")
  return(tObject)
})

setMethod("Visualize_Essence",'ContrastC', function(.Object, filter, lens){
  appel = lens
  print("Trying To Produce a Visualization...")
  if (dim(appel)[1] < 1 ){ return()}
  Contrasts2 = renderPlot({ 
    par(mfrow=c(1,2))
    twoord.plot(appel$sequence,appel$AddictiveRate,
                appel$sequence,appel$DiscountSums, type='l', lcol = 'red', 
                rcol = 'green', ylab = 'AddictiveRate', rylab = 'DiscountSums',
                xlab = 'Time') #, xticklab = appel$DATE, xtickpos = appel$sequence
    twoord.plot(appel$sequence,appel$AddictiveRate,
                appel$sequence,appel$FavorableSums, type='l', lcol = 'red', 
                rcol = 'green', ylab = 'AddictiveRate', rylab = 'FavorableSums',
                xlab = 'Time') #, xticklab = appel$DATE) #, xtickpos = appel$sequence
    return(Contrasts2)
  })
})
