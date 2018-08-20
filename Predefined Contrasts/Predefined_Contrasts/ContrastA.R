setClass('ContrastA', contains = 'Contrasts')
setMethod("initialize",'ContrastA', function(.Object)
{
  .Object@ParameterNames = c('WagersCounts','balance')
  .Object@SelectedCharacteristics = c('WagersCounts','balance')
  .Object@Header = new('Contrasts')@Header
  .Object
}
)

#selected = subset(Performance3MData,CommissionableSums > input$variable1 & balance < -input$variable2)
#selected$impervious = selected$CommissionableSums * selected$balance




setMethod('setSelection','ContrastA', function(tObject, dataset, input)
{
  callNextMethod(tObject,dataset,input)
  #print("That was that")
  tObject@Selected <- subset(dataset,CommissionableSums > input$variable1 & balance < -input$variable2)
  tObject@Selected$impervious <- tObject@Selected$CommissionableSums * tObject@Selected$balance
  #print("Assigning Names")
  tObject@Websites <- rownames(tObject@Selected)
  return(tObject)
})

setMethod("Visualize_Essence",'ContrastA', function(.Object, filter, lens){
  appel = lens
  #print("Trying To Produce a Visualization...")
  
  if (dim(appel)[1] < 1 ){ return()}
  Contrasts1 = renderPlot({ 
    twoord.plot(appel$sequence,appel$balance,
                appel$sequence,appel$WagersCounts, type='l', lcol = 'red', 
                rcol = 'green', ylab = 'Balance', rylab = 'CommissionableSums',
                xlab = 'Time', xtickpos = appel$sequence, xticklab = appel$DATE) 
  })
  return(Contrasts1)
})
