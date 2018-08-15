
Data = read.table(file.choose(),sep=',',header=TRUE, stringsAsFactors = FALSE)

for (k in c(3:dim(Data)[2]))
{
  Data[,k] = as.numeric(Data[,k])
}




###Standardize per active user:

Data$sequence = as.integer( as.Date(Data$DATE ) - min(as.Date(Data$DATE)))
Data$PayoffUser = Data$PayoffSums / Data$MemberCounts
Data$PayoffBet = Data$PayoffSums / Data$WagersCounts
Data$CommissionableUser = Data$CommissionableSums / Data$MemberCounts
Data$CommissionableBet = Data$CommissionableSums / Data$WagersCounts
Data$BitePerUser = Data$Bite / Data$MemberCounts
Data$BitePerBet = Data$Bite / Data$WagersCounts
Data$FavorableUser = Data$FavorableSums / Data$MemberCounts
Data$FavorableBet = Data$FavorableSums / Data$WagersCounts
Data$DiscountUser = Data$DiscountSums  / Data$MemberCounts
Data$DiscountBets = Data$DiscountSums  / Data$WagersCounts
Data$OthersUser   = Data$OthersSums / Data$MemberCounts
Data$OthersBet    = Data$OthersSums / Data$WagersCounts
Data$MemberDiscountBet  = Data$MemberDiscountSums / Data$WagersCounts
Data$MemberDiscountUser = Data$MemberDiscountSums / Data$MemberCounts
Data$ThirdPartyPaymentUser = Data$ThirdPartyPaymentSums / Data$MemberCounts
Data$ThirdPartyPaymentBet = Data$ThirdPartyPaymentSums / Data$MemberCounts
Data$ThirdPartyPaymentPTime = Data$ThirdPartyPaymentSums / Data$ThirdPartyPaymentTimes
Data$CreditDepositUser = Data$CreditDepositSums / Data$MemberCounts
Data$CreditDepositBet = Data$CreditDepositSums / Data$WagersCounts
Data$CreditDepositPTime = Data$CreditDepositSums / Data$CreditDepositTimes
Data$WithdrawUser = Data$WithdrawAmount / Data$MemberCounts
Data$WithdrawBet = Data$WithdrawAmount / Data$WagersCounts
Data$CreditWithdrawUser = Data$CreditWithdrawSums / Data$MemberCounts
Data$CreditWithdrawBet = Data$CreditWithdrawSums / Data$WagersCounts
Data$CreditWithdrawPTime = Data$CreditWithdrawSums / Data$CreditWithdrawTimes
Data$FirstDepositUser = Data$FirstDepositAmount / Data$MemberCounts
Data$FirstDepositBet  = Data$FirstDepositAmount / Data$WagersCounts
Data$FirstDepositpTime = Data$FirstDepositAmount / Data$FirstDepositNumber
Data$OWithdrawPTime = Data$WithdrawSums / Data$WithdrawTimes
Data$OWithdrawUser = Data$WithdrawSums / Data$MemberCounts
Data$OWithdrawBet = Data$WithdrawSums / Data$WagersCounts
Data$CDepositPTime = Data$DepositSums / Data$DepositTimes
Data$CDepositPUser = Data$DepositSums / Data$MemberCounts
Data$CDepositBet = Data$DepositSums / Data$WagersCounts
Data$DepositUser = Data$DepositAmount / Data$MemberCounts
Data$DepositBet = Data$DepositAmount / Data$WagersCounts

Data[is.na(Data)] = 0.0






###Standardize some variables:
Data[ ,c("AddictiveRate","balance","BetAmountSums","Bite","BitePerBet",
         "BitePerUser","ChurnRate1","ChurnRate30","ChurnRate7","CommissionableBet",
         "CommissionableSums","CommissionableUser","CreditDepositBet","CreditDepositPTime","CreditDepositSums",
         "CreditDepositTimes","CreditDepositUser","CreditWithdrawBet","CreditWithdrawPTime","CreditWithdrawSums",
         "CreditWithdrawTimes","CreditWithdrawUser","DepositAmount","DepositSums",
         "DepositTimes","DiscountBets","DiscountSums","DiscountUser","FavorableBet",
         "FavorableSums","FavorableUser","FirstDepositAmount","FirstDepositBet","FirstDepositNumber",
         "FirstDepositpTime","FirstDepositUser", "MemberCounts","MemberDiscountBet","MemberDiscountSums",
         "MemberDiscountUser","OthersBet","OthersSums","OthersUser","PayoffBet",
         "PayoffSums","PayoffUser","RetentionRate1","RetentionRate30","RetentionRate7",
         "ThirdPartyPaymentBet","ThirdPartyPaymentPTime","ThirdPartyPaymentSums",
         "ThirdPartyPaymentTimes","ThirdPartyPaymentUser","WagersCounts","WithdrawAmount","WithdrawBet", 
         "WithdrawPTime","WithdrawSums","WithdrawTimes","WithdrawUser")]

data_mean = aggregate( Data[,c("AddictiveRate","balance","BetAmountSums","Bite","BitePerBet","BitePerUser",
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
                               "WithdrawUser")], by= list(Data$SystemCode), FUN='mean')
          
data_var  = aggregate( Data[,c("AddictiveRate","balance","BetAmountSums","Bite","BitePerBet","BitePerUser",
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
                               "WithdrawUser")], by= list(Data$SystemCode), FUN='sd')

data_var[ data_var == 0 ] = 1


Data2 = Data
for ( category_id in Data$SystemCode)
{
  Data2[Data$SystemCode == category_id,c("AddictiveRate","balance","BetAmountSums","Bite","BitePerBet",
                                         "BitePerUser","ChurnRate1","ChurnRate30","ChurnRate7","CommissionableBet",
                                         "CommissionableSums","CommissionableUser","CreditDepositBet","CreditDepositPTime","CreditDepositSums",
                                         "CreditDepositTimes","CreditDepositUser","CreditWithdrawBet","CreditWithdrawPTime","CreditWithdrawSums",
                                         "CreditWithdrawTimes","CreditWithdrawUser","DepositAmount","DepositSums",
                                         "DepositTimes","DiscountBets","DiscountSums","DiscountUser","FavorableBet",
                                         "FavorableSums","FavorableUser","FirstDepositAmount","FirstDepositBet","FirstDepositNumber",
                                         "FirstDepositpTime","FirstDepositUser", "MemberCounts","MemberDiscountBet","MemberDiscountSums",
                                         "MemberDiscountUser","OthersBet","OthersSums","OthersUser","PayoffBet",
                                         "PayoffSums","PayoffUser","RetentionRate1","RetentionRate30","RetentionRate7",
                                         "ThirdPartyPaymentBet","ThirdPartyPaymentPTime","ThirdPartyPaymentSums",
                                         "ThirdPartyPaymentTimes","ThirdPartyPaymentUser","WagersCounts","WithdrawAmount","WithdrawBet", 
                                         "WithdrawPTime","WithdrawSums","WithdrawTimes","WithdrawUser")] =
                                       ((Data[Data$SystemCode == category_id, c("AddictiveRate","balance","BetAmountSums","Bite","BitePerBet",
                                                                                "BitePerUser","ChurnRate1","ChurnRate30","ChurnRate7","CommissionableBet",
                                                                                "CommissionableSums","CommissionableUser","CreditDepositBet","CreditDepositPTime","CreditDepositSums",
                                                                                "CreditDepositTimes","CreditDepositUser","CreditWithdrawBet","CreditWithdrawPTime","CreditWithdrawSums",
                                                                                "CreditWithdrawTimes","CreditWithdrawUser","DepositAmount","DepositSums",
                                                                                "DepositTimes","DiscountBets","DiscountSums","DiscountUser","FavorableBet",
                                                                                "FavorableSums","FavorableUser","FirstDepositAmount","FirstDepositBet","FirstDepositNumber",
                                                                                "FirstDepositpTime","FirstDepositUser", "MemberCounts","MemberDiscountBet","MemberDiscountSums",
                                                                                "MemberDiscountUser","OthersBet","OthersSums","OthersUser","PayoffBet",
                                                                                "PayoffSums","PayoffUser","RetentionRate1","RetentionRate30","RetentionRate7",
                                                                                "ThirdPartyPaymentBet","ThirdPartyPaymentPTime","ThirdPartyPaymentSums",
                                                                                "ThirdPartyPaymentTimes","ThirdPartyPaymentUser","WagersCounts","WithdrawAmount","WithdrawBet", 
                                                                                "WithdrawPTime","WithdrawSums","WithdrawTimes","WithdrawUser")] - 
                                        c(data_mean[data_mean$Group.1 == category_id,c("AddictiveRate","balance","BetAmountSums","Bite","BitePerBet",
                                                                                       "BitePerUser","ChurnRate1","ChurnRate30","ChurnRate7","CommissionableBet",
                                                                                       "CommissionableSums","CommissionableUser","CreditDepositBet","CreditDepositPTime","CreditDepositSums",
                                                                                       "CreditDepositTimes","CreditDepositUser","CreditWithdrawBet","CreditWithdrawPTime","CreditWithdrawSums",
                                                                                       "CreditWithdrawTimes","CreditWithdrawUser","DepositAmount","DepositSums",
                                                                                       "DepositTimes","DiscountBets","DiscountSums","DiscountUser","FavorableBet",
                                                                                       "FavorableSums","FavorableUser","FirstDepositAmount","FirstDepositBet","FirstDepositNumber",
                                                                                       "FirstDepositpTime","FirstDepositUser", "MemberCounts","MemberDiscountBet","MemberDiscountSums",
                                                                                       "MemberDiscountUser","OthersBet","OthersSums","OthersUser","PayoffBet",
                                                                                       "PayoffSums","PayoffUser","RetentionRate1","RetentionRate30","RetentionRate7",
                                                                                       "ThirdPartyPaymentBet","ThirdPartyPaymentPTime","ThirdPartyPaymentSums",
                                                                                       "ThirdPartyPaymentTimes","ThirdPartyPaymentUser","WagersCounts","WithdrawAmount","WithdrawBet", 
                                                                                       "WithdrawPTime","WithdrawSums","WithdrawTimes","WithdrawUser")]
                                        )) /
                                        c(data_var[data_var$Group.1 == category_id, c("AddictiveRate","balance","BetAmountSums","Bite","BitePerBet",
                                                                                      "BitePerUser","ChurnRate1","ChurnRate30","ChurnRate7","CommissionableBet",
                                                                                      "CommissionableSums","CommissionableUser","CreditDepositBet","CreditDepositPTime","CreditDepositSums",
                                                                                      "CreditDepositTimes","CreditDepositUser","CreditWithdrawBet","CreditWithdrawPTime","CreditWithdrawSums",
                                                                                      "CreditWithdrawTimes","CreditWithdrawUser","DepositAmount","DepositSums",
                                                                                      "DepositTimes","DiscountBets","DiscountSums","DiscountUser","FavorableBet",
                                                                                      "FavorableSums","FavorableUser","FirstDepositAmount","FirstDepositBet","FirstDepositNumber",
                                                                                      "FirstDepositpTime","FirstDepositUser", "MemberCounts","MemberDiscountBet","MemberDiscountSums",
                                                                                      "MemberDiscountUser","OthersBet","OthersSums","OthersUser","PayoffBet",
                                                                                      "PayoffSums","PayoffUser","RetentionRate1","RetentionRate30","RetentionRate7",
                                                                                      "ThirdPartyPaymentBet","ThirdPartyPaymentPTime","ThirdPartyPaymentSums",
                                                                                      "ThirdPartyPaymentTimes","ThirdPartyPaymentUser","WagersCounts","WithdrawAmount","WithdrawBet", 
                                                                                      "WithdrawPTime","WithdrawSums","WithdrawTimes","WithdrawUser")]
                                        ))
}
  



#test of performance
apply( Data2[Data$SystemCode == category_id,c('FavorableSums','DiscountSums',
                                              'OthersSums','MemberDiscountSums')], 2, mean)
apply( Data2[Data$SystemCode == category_id,c('FavorableSums','DiscountSums',
                                              'OthersSums','MemberDiscountSums')], 2, var)


summary( lmer( ChurnRate1 ~ 1  + FavorableSums + DiscountSums + OthersSums + MemberDiscountSums + 
                 ThirdPartyPaymentSums + CreditDepositSums + DepositSums + WithdrawSums +
                 CreditWithdrawSums + FirstDepositNumber + FirstDepositAmount +
                 DepositAmount + WithdrawAmount + balance + sequence + (1 | SystemCode), data = Data)) 
         
summary( lmer( ChurnRate1 ~ 1  + FavorableSums + DiscountSums + OthersSums + MemberDiscountSums + 
                 ThirdPartyPaymentSums + CreditDepositSums + DepositSums + WithdrawSums +
                 CreditWithdrawSums + FirstDepositNumber + FirstDepositAmount +
                 DepositAmount + WithdrawAmount + balance + sequence + (1 | SystemCode), data = Data2)) 

model1 =
        step(lmer( ChurnRate1 ~ 1  + FavorableSums + DiscountSums + OthersSums + MemberDiscountSums + 
        ThirdPartyPaymentSums + CreditDepositSums + DepositSums + WithdrawSums +
        CreditWithdrawSums + FirstDepositNumber + FirstDepositAmount +
        DepositAmount + WithdrawAmount + balance + sequence + (1 | SystemCode), data = Data2),
        direction='backward', trace=TRUE)
 
model2 =
  step(lmer( ChurnRate1 ~ 1  + FavorableSums + DiscountSums + OthersSums + MemberDiscountSums + 
               ThirdPartyPaymentSums + CreditDepositSums + DepositSums + WithdrawSums +
               CreditWithdrawSums + FirstDepositNumber + FirstDepositAmount +
               DepositAmount + WithdrawAmount + balance + sequence + (1 | SystemCode), data = Data2),
       direction='forward', trace=TRUE)

model3 =
  step(lmer( ChurnRate1 ~ (1  + FavorableSums + DiscountSums + OthersSums + MemberDiscountSums + 
               ThirdPartyPaymentSums + CreditDepositSums + DepositSums + WithdrawSums +
               CreditWithdrawSums + FirstDepositNumber + FirstDepositAmount + 
               DepositAmount + WithdrawAmount + balance + sequence) + 
               (1 + FavorableSums + DiscountSums + OthersSums + MemberDiscountSums + 
                  ThirdPartyPaymentSums + CreditDepositSums + DepositSums + WithdrawSums +
                  CreditWithdrawSums + FirstDepositNumber + FirstDepositAmount + 
                  DepositAmount + WithdrawAmount + balance + sequence | SystemCode), data = Data2),
       direction='forward', trace=TRUE, alpha.random = 0.001, alpha.fixed = 0.001, scale = 1)           

model3 =
  step(lmer( ChurnRate1 ~ (1  + FavorableSums + DiscountSums + OthersSums + MemberDiscountSums + 
                             ThirdPartyPaymentSums + CreditDepositSums + DepositSums + WithdrawSums +
                             CreditWithdrawSums + FirstDepositNumber + FirstDepositAmount +
                             DepositAmount + WithdrawAmount + balance + sequence + 
                             PayoffSums + CommissionableSums + WagersCounts + Bite + MemberCounts + BetAmountSums +
                             RetentionRate1 + AddictiveRate
                           ) + ( 1  + FavorableSums + DiscountSums + OthersSums + MemberDiscountSums + 
                                   ThirdPartyPaymentSums + CreditDepositSums + DepositSums + WithdrawSums +
                                   CreditWithdrawSums + FirstDepositNumber + FirstDepositAmount +
                                   DepositAmount + WithdrawAmount + balance + sequence + 
                                   PayoffSums + CommissionableSums + WagersCounts + Bite + MemberCounts + BetAmountSums +
                                   RetentionRate1 + AddictiveRate
                             | SystemCode), data = Data2),
       direction='both', trace=FALSE, alpha.random = 0.001, alpha.fixed = 0.001, scale = 2) 


parameters = (labels(model3$fixed)[[1]])[ !model3$fixed$Eliminated]





apply( xmmer[,c('FavorableSums','DiscountSums',
                                              'OthersSums','MemberDiscountSums')], 2, mean)
apply( xmmer[,c('FavorableSums','DiscountSums',
                                              'OthersSums','MemberDiscountSums')], 2, var)

