#library(data.table)


setClass('Contrasts',slots = c( 
  ParameterNames = "character" , 
  SelectedCharacteristics = "character",
  Header = "character",
  Selected = "data.frame",
  Websites = "character"
)
)

setGeneric('getParameterName', function(.Object){ return('character')})
setGeneric('getSelectedCharacteristics', function(.Object){ return('character')})
setGeneric("setSelection", function(tObject, dataset, input){ return('Contrasts')})
setGeneric("getSelection", function(tObject){return("data.frame")})
setGeneric("getFiltered", function(.Object, input){return('data.frame')})
setGeneric("getSites", function(.Object){return('character')})
setGeneric("Visualize_Essence", function(.Object, filter, lens){return('plot')})
setGeneric("SetParameters", function(.Object, param1){return('Contrasts')})

setMethod("initialize",'Contrasts', function(.Object)
{
  print("initializing...")
  .Object@Header = c("AddictiveRate","balance","BetAmountSums","Bite","BitePerBet","BitePerUser",
                     "CDepositBet","CDepositPTime","CDepositPUser","ChurnRate1","ChurnRate30",
                     "ChurnRate7","CommissionableBet","CommissionableSums","CommissionableUser",
                     "CreditDepositBet","CreditDepositPTime","CreditDepositSums","CreditDepositTimes",
                     "CreditDepositUser","CreditWithdrawBet","CreditWithdrawPTime",
                     "CreditWithdrawSums","CreditWithdrawTimes","CreditWithdrawUser",
                     "DepositAmount","DepositBet","DepositSums","DepositTimes","DepositUser",
                     "DiscountBets","DiscountSums","DiscountUser","FavorableBet","FavorableSums",
                     "FavorableUser","FirstDepositAmount","FirstDepositBet","FirstDepositNumber",
                     "FirstDepositpTime","FirstDepositUser","MemberCounts","MemberDiscountBet",
                     "MemberDiscountSums","MemberDiscountUser","OthersBet","OthersSums",
                     "OthersUser","OWithdrawBet","OWithdrawPTime","OWithdrawUser","PayoffBet",
                     "PayoffSums","PayoffUser","RetentionRate1","RetentionRate30","RetentionRate7"
                     ,"ThirdPartyPaymentBet","ThirdPartyPaymentPTime",
                     "ThirdPartyPaymentSums","ThirdPartyPaymentTimes","ThirdPartyPaymentUser",
                     "WagersCounts","WithdrawAmount","WithdrawBet","WithdrawSums","WithdrawTimes",
                     "WithdrawUser","impervious")
  .Object@Selected <- as.data.frame(setNames(replicate(length(.Object@Header),
                                                       0, simplify = F), 
                                             .Object@Header))
  .Object@Websites <- c('')
  return(.Object)
}
)



setMethod('getParameterName', 'Contrasts', function(.Object)
{
  return( .Object@ParameterNames)
}
)
setMethod('getSelectedCharacteristics', 'Contrasts', function(.Object)
{
  return( .Object@SelectedCharacteristics)
}
)

setMethod('setSelection','Contrasts', function(tObject, dataset, input)
{
  conditionOne = is.data.frame(dataset)
  conditionTwo = is.list(input)
  conditionThree = is.numeric(input$variable1)
  conditionFour  = is.numeric(input$variable2)
  conditionFive  = prod(tObject@Header %in% colnames(dataset))
  if (( conditionOne * conditionTwo * conditionThree * conditionFour * conditionFive) < 1)
  {
    return(tObject)
  } else if (input$Contrast == 'ContrastZZ')
  {
    print("ContrastZZ")
    print (input$CompareVariable1)
    print (input$CompareVariable2)
    return(tObject)
  }else {
    print("Everything went fine")
  }
  
})

setMethod('getSelection','Contrasts', function(tObject)
{
  return(tObject@Selected)
})


setMethod('getSites','Contrasts',function(.Object)
{
  print("Getting Values")
  return(.Object@Websites)
}
)

#need to add validation
setMethod("getFiltered",'Contrasts', function(.Object, input)
{
  processed_input = input[ input$SystemCode %in% .Object@Websites,]
  processed_input = processed_input[ order(processed_input$SystemCode, processed_input$sequence), ]
  return( processed_input )
})

setGeneric("SetParameters", function(.Object, param1, param2){return('Contrasts')})
setMethod("SetParameters",'Contrasts', function(.Object,param1)
  {
   .Object@ParameterNames = param1
   .Object@SelectedCharacteristics = param1
   return(.Object)
  })