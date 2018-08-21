setClass('ContrastE', contains = 'Contrasts')
setMethod("initialize",'ContrastE', function(.Object)
{
  .Object@ParameterNames =c('DepositUser','DepositAmount')
  .Object@SelectedCharacteristics = c('DepositUser','DepositAmount')
  .Object@Header = new('Contrasts')@Header
  .Object
}
)


setMethod('setSelection','ContrastE', function(tObject, dataset, input)
{
  callNextMethod(tObject,dataset,input)
  print("That was that")
  candidate1 = subset(dataset,DepositUser > input$variable1 & DepositAmount > input$variable1)
  candidate1$impervious = candidate1$DepositUser * candidate1$DepositAmount
  candidate1$rn = rownames(candidate1)
  candidate2 = subset(dataset,DepositUser < input$variable2 & DepositAmount < input$variable2)
  candidate2$impervious = - candidate2$DepositUser * candidate2$DepositAmount
  candidate2$rn = rownames(candidate2)
  tryCatch(
    {
      selected = rbind( as.data.frame(candidate1), as.data.frame(candidate2))
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

setMethod("Visualize_Essence",'ContrastE', function(.Object, filter, lens){
  appel = lens
  print("Trying To Produce a Visualization...")
  if (dim(appel)[1] < 1 ){ return()}
  Contrasts2 = renderPlot({ 
    twoord.plot(appel$sequence,appel$DepositUser,
                appel$sequence,appel$DepositAmount, type='l', lcol = 'red', 
                rcol = 'green', ylab = 'Deposit Times', rylab = 'Deposit Amounts',
                xlab = 'Time') #, xticklab = appel$DATE, xtickpos = appel$sequence
    return(Contrasts2)
  })
})

