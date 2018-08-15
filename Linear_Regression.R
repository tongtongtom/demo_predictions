library(lme4)
library(lmerTest)


#[1] "AddictiveRate"          "balance"                "BetAmountSums"          "Bite"                   "BitePerBet"            
#[6] "BitePerUser"            "ChurnRate1"             "ChurnRate30"            "ChurnRate7"             "CommissionableBet"     
#[11] "CommissionableSums"     "CommissionableUser"     "CreditDepositBet"       "CreditDepositPTime"     "CreditDepositSums"     
#[16] "CreditDepositTimes"     "CreditDepositUser"      "CreditWithdrawBet"      "CreditWithdrawPTime"    "CreditWithdrawSums"    
#[21] "CreditWithdrawTimes"    "CreditWithdrawUser"     "DATE"                   "DepositAmount"          "DepositSums"           
#[26] "DepositTimes"           "DiscountBets"           "DiscountSums"           "DiscountUser"           "FavorableBet"          
#[31] "FavorableSums"          "FavorableUser"          "FirstDepositAmount"     "FirstDepositBet"        "FirstDepositNumber"    
#[36] "FirstDepositpTime"      "FirstDepositUser"       "MemberCounts"           "MemberDiscountBet"      "MemberDiscountSums"    
#[41] "MemberDiscountUser"     "OthersBet"              "OthersSums"             "OthersUser"             "PayoffBet"             
#[46] "PayoffSums"             "PayoffUser"             "RetentionRate1"         "RetentionRate30"        "RetentionRate7"        
#[51] "sequence"               "SystemCode"             "ThirdPartyPaymentBet"   "ThirdPartyPaymentPTime" "ThirdPartyPaymentSums" 
#[56] "ThirdPartyPaymentTimes" "ThirdPartyPaymentUser"  "WagersCounts"           "WithdrawAmount"         "WithdrawBet"           
#[61] "WithdrawPTime"          "WithdrawSums"           "WithdrawTimes"          "WithdrawUser"          


model = list()
model$AddictiveRate = lmer(AddictiveRate ~ 1 + sequence + (1 + sequence | SystemCode), data = Data2 , REML=FALSE ) 
model$balance       = lmer(balance ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 ) 
model$BetAmountSums = lmer(BetAmountSums ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 ) 
model$Bite          = lmer(Bite ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 ) 
model$BitePerUser   = lmer(BitePerUser ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$BitePerBet    = lmer(BitePerBet ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$ChurnRate1    = lmer(ChurnRate1 ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$ChurnRate7    = lmer(ChurnRate7 ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$ChurnRate30   = lmer(ChurnRate30 ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$CommissionableSums = lmer(CommissionableSums ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$CommissionableUser = lmer(CommissionableUser ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$CommissionableBet  = lmer(CommissionableBet ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$CreditDepositBet   = lmer(CreditDepositBet ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$CreditDepositPTime = lmer(CreditDepositPTime ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$CreditDepositSums  = lmer(CreditDepositSums ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$CreditDepositTimes = lmer(CreditDepositTimes ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$CreditDepositUser  = lmer(CreditDepositUser ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$CreditWithdrawBet  = lmer(CreditWithdrawBet ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$CreditWithdrawPTime= lmer(CreditWithdrawPTime ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$CreditWithdrawSums = lmer(CreditWithdrawSums ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$CreditWithdrawTimes= lmer(CreditWithdrawTimes ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$CreditWithdrawUser = lmer(CreditWithdrawUser ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$DepositAmount = lmer(DepositAmount ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$DepositSums   = lmer(DepositSums ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$DepositTimes  = lmer(DepositTimes ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$DiscountBets = lmer(DiscountBets ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$DiscountSums   = lmer(DiscountSums ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$DiscountUser  = lmer(DiscountUser ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$FavorableBet = lmer(FavorableBet ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$FavorableSums   = lmer(FavorableSums ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$FavorableUser  = lmer(FavorableUser ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$FirstDepositAmount = lmer(FirstDepositAmount ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$FirstDepositBet    = lmer(FirstDepositBet ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$FirstDepositNumber = lmer(FirstDepositNumber ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$FirstDepositpTime  = lmer(FirstDepositpTime ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$FirstDepositUser   = lmer(FirstDepositUser ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$MemberCounts       = lmer(MemberCounts ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$MemberDiscountBet  = lmer(MemberDiscountBet ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$MemberDiscountSums = lmer(MemberDiscountSums ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$MemberDiscountUser = lmer(MemberDiscountUser ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$OthersBet  = lmer(OthersBet ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$OthersSums = lmer(OthersSums ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$OthersUser = lmer(OthersUser ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$PayoffBet  = lmer(PayoffSums ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$PayoffSums = lmer(PayoffBet  ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$PayoffUser = lmer(PayoffUser ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$RetentionRate1  = lmer(RetentionRate1 ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$RetentionRate7  = lmer(RetentionRate1 ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$RetentionRate30 = lmer(RetentionRate1 ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$ThirdPartyPaymentBet   = lmer(ThirdPartyPaymentBet   ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$ThirdPartyPaymentPTime = lmer(ThirdPartyPaymentPTime ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$ThirdPartyPaymentSums  = lmer(ThirdPartyPaymentSums  ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$ThirdPartyPaymentTimes = lmer(ThirdPartyPaymentTimes ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$ThirdPartyPaymentUser  = lmer(ThirdPartyPaymentUser ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$WagersCounts  = lmer(WagersCounts ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$WithdrawAmount  = lmer(WithdrawAmount ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$WithdrawBet     = lmer(WithdrawBet    ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
#model$WithdrawPTime   = lmer(WithdrawPTime  ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$WithdrawSums    = lmer(WithdrawSums   ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$WithdrawTimes   = lmer(WithdrawTimes  ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )
model$WithdrawUser    = lmer(WithdrawUser   ~ 1 + sequence + (1+sequence | SystemCode), data = Data2 )

fctr = 'balance'
data_template = data.frame(t(extract_line(fctr)))
data_template = data_template[0,]


for (fctr in names(model))
{
  data_template = rbind(unlist(t(extract_line(fctr))[2,]), data_template)
}
colnames(data_template) = rownames(extract_line(fctr))
rownames(data_template) = names(model)


extract_line = function( fctr) {
  randoms  = ranef(model[[fctr]])$SystemCode
  fixedefs = fixef(model[[fctr]])
  return( randoms + fixedefs )
}

