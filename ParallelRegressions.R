regressionlist=c("AddictiveRate","balance","BetAmountSums","Bite","BitePerBet","BitePerUser",
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
                 "WithdrawUser")    
Data2[is.na(Data2)] = 0.0
fillzero = matrix(0, nrow= length(unique(Data2$SystemCode)), ncol = length(regressionlist))
fillzero = as.data.frame(fillzero)
colnames(fillzero) = regressionlist
rownames(fillzero) = unique(Data2$SystemCode)

for (k in unique(Data2$SystemCode))
{
  analys_dat = Data2[Data2$SystemCode == k,]
  if (dim(analys_dat)[1] > 1)
  {
    for( param in regressionlist)
    {
      tryCatch({
      temp_model = eval(parse(text = paste("lm(", param, "~ sequence, data= analys_dat)",sep='')))
      fillzero[k,param] = unlist((temp_model$coefficients)['sequence'])
      } , warning = function(warning_condition){
        stop(warning_condition)
      })
    }
  }
}
write.table(fillzero, file = "D:/source/1_Predictive/3months_performance" ,sep=';')
write.table(Data, file = "D:/source/1_Predictive/raw_data" ,sep=';')

