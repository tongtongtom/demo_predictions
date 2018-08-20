standardizeDataSet = function(Data)
{
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
      Data2[Data$SystemCode == category_id,c("AddictiveRate","balance","BetAmountSums","Bite","BitePerBet","BitePerUser",
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
                                         "WithdrawUser")] =
      ((Data[Data$SystemCode == category_id, c("AddictiveRate","balance","BetAmountSums","Bite","BitePerBet","BitePerUser",
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
                                             "WithdrawUser")] - 
        c(data_mean[data_mean$Group.1 == category_id,c("AddictiveRate","balance","BetAmountSums","Bite","BitePerBet","BitePerUser",
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
                                                       "WithdrawUser")]
        )) /
        c(data_var[data_var$Group.1 == category_id, c("AddictiveRate","balance","BetAmountSums","Bite","BitePerBet","BitePerUser",
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
                                                     "WithdrawUser")]
       ))
  }

  #test of performance
  print(apply( Data2[Data$SystemCode == category_id,c('FavorableSums','DiscountSums',
                                              'OthersSums','MemberDiscountSums')], 2, mean))
  print(apply( Data2[Data$SystemCode == category_id,c('FavorableSums','DiscountSums',
                                              'OthersSums','MemberDiscountSums')], 2, var))

  return(Data2)
}
