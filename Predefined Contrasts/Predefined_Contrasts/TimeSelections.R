##Insert the values that are not in the dataset
###And Remove Datasets with less than 5 days of data

excluded = list()
firsta  = numeric()
seconda = numeric()
thirda  = numeric()
fourtha = numeric()
fiftha  = numeric()
kept    = numeric()

for (input in unique(Data2$SystemCode))
{
  tempdata = (Data2[Data2$SystemCode == input,])
  if( dim(tempdata)[1] > 5) { kept = append(kept, input)  }
  else { 
    switch( dim(tempdata)[1] ,
          { firsta =  append(firsta, input) },
          { seconda = append(seconda,input) },
          { thirda  = append(thirda, input) },
          { fourtha = append(fourtha,input) },
          { fiftha  = append(fiftha, input) })
  }  
}


datelookup = data.frame(unique(Data[,c('sequence','DATE')]))

newdata = Data[0,]
for ( k in kept)
{
  tempsubset = (Data[ Data$SystemCode == k,])
  datarange = c(min(tempsubset$sequence):max(tempsubset$sequence))
  
  sequencedat = data.frame(datarange)
  colnames(sequencedat) = c('sequence')
  looks = merge(sequencedat,datelookup )
  tmpdataframe = merge(looks,tempsubset, all.x =TRUE)
  tmpdataframe[ is.na(tmpdataframe) ] = 0.0
  tmpdataframe$SystemCode = k
  newdata = rbind(newdata,tmpdataframe)
}

days7set = newdata[0,]
###A Dataset of at least 10 days:
for (remp in unique(newdata$SystemCode))
{
  subsetted =  newdata[newdata$SystemCode == remp,]
  if (dim(subsetted[subsetted$SystemCode == remp,])[1] > 7)
  {
    full_subset = subsetted[subsetted$sequence %in% (max(subsetted$sequence)-8) : max(subsetted$sequence),]
    days7set = rbind(days7set, full_subset )
  }
}



days30set = newdata[0,]
###B A Dataset of 30 days:
for (remp in unique(newdata$SystemCode))
{
  subsetted =  newdata[newdata$SystemCode == remp,]
  if (dim(subsetted[subsetted$SystemCode == remp,])[1] > 30)
  {
    full_subset = subsetted[subsetted$sequence %in% (max(subsetted$sequence)-31) : max(subsetted$sequence),]
    days30set = rbind(days30set, full_subset )
  }
}




days90set = newdata[0,]
###C A dataset of 90 days:
for (remp in unique(newdata$SystemCode))
{
  subsetted =  newdata[newdata$SystemCode == remp,]
  if (dim(subsetted[subsetted$SystemCode == remp,])[1] > 90)
  {
    full_subset = subsetted[subsetted$sequence %in% (max(subsetted$sequence)-91) : max(subsetted$sequence),]
    days90set = rbind(days90set, full_subset )
  }
}

days7set  = days7set[order(days7set$SystemCode,days7set$sequence),]
days30set = days30set[order(days30set$SystemCode,days30set$sequence),]
days90set = days90set[order(days90set$SystemCode,days90set$sequence),]
days7stand  = standardizeDataSet(days7set)
days30stand = standardizeDataSet(days30set)
days90stand  = standardizeDataSet(days90set)

reg7set = ParallelSTDRegression(days7set)
reg7stand = ParallelRegression(days7stand)
reg30set = ParallelSTDRegression(days30set)
reg30stand = ParallelRegression(days30stand)
reg90set = ParallelSTDRegression(days90set)
reg90stand = ParallelRegression(days90stand)


write.table(reg7set, file = "D:/source/1_Predictive/Predefined Contrasts/Data/reg7dset" ,sep=';')
write.table(days7set, file = "D:/source/1_Predictive/Predefined Contrasts/Data/days7dset" ,sep=';')
write.table(reg7stand, file = "D:/source/1_Predictive/Predefined Contrasts/Data/reg7stand" ,sep=';')
write.table(days7stand, file = "D:/source/1_Predictive/Predefined Contrasts/Data/days7stand" ,sep=';')

write.table(reg30set, file = "D:/source/1_Predictive/Predefined Contrasts/Data/reg30dset" ,sep=';')
write.table(days30set, file = "D:/source/1_Predictive/Predefined Contrasts/Data/days30dset" ,sep=';')
write.table(reg30stand, file = "D:/source/1_Predictive/Predefined Contrasts/Data/reg30stand" ,sep=';')
write.table(days30stand, file = "D:/source/1_Predictive/Predefined Contrasts/Data/days30stand" ,sep=';')

write.table(reg90set, file = "D:/source/1_Predictive/Predefined Contrasts/Data/reg90dset" ,sep=';')
write.table(days90set, file = "D:/source/1_Predictive/Predefined Contrasts/Data/days90dset" ,sep=';')
write.table(reg90stand, file = "D:/source/1_Predictive/Predefined Contrasts/Data/reg90stand" ,sep=';')
write.table(days90stand, file = "D:/source/1_Predictive/Predefined Contrasts/Data/days90stand" ,sep=';')

#[1] "AddictiveRate"          "balance"                "BetAmountSums"          "Bite"                  
#[5] "BitePerBet"             "BitePerUser"            "CDepositBet"            "CDepositPTime"         
#[9] "CDepositPUser"          "ChurnRate1"             "ChurnRate30"            "ChurnRate7"            
#[13] "CommissionableBet"      "CommissionableSums"     "CommissionableUser"     "CreditDepositBet"      
#[17] "CreditDepositPTime"     "CreditDepositSums"      "CreditDepositTimes"     "CreditDepositUser"     
#[21] "CreditWithdrawBet"      "CreditWithdrawPTime"    "CreditWithdrawSums"     "CreditWithdrawTimes"   
#[25] "CreditWithdrawUser"     "DepositAmount"          "DepositBet"             "DepositSums"           
#[29] "DepositTimes"           "DepositUser"            "DiscountBets"           "DiscountSums"          
#[33] "DiscountUser"           "FavorableBet"           "FavorableSums"          "FavorableUser"         
#[37] "FirstDepositAmount"     "FirstDepositBet"        "FirstDepositNumber"     "FirstDepositpTime"     
#[41] "FirstDepositUser"       "MemberCounts"           "MemberDiscountBet"      "MemberDiscountSums"    
#[45] "MemberDiscountUser"     "OthersBet"              "OthersSums"             "OthersUser"            
#[49] "OWithdrawBet"           "OWithdrawPTime"         "OWithdrawUser"          "PayoffBet"             
#[53] "PayoffSums"             "PayoffUser"             "RetentionRate1"         "RetentionRate30"       
#[57] "RetentionRate7"         "ThirdPartyPaymentBet"   "ThirdPartyPaymentPTime" "ThirdPartyPaymentSums" 
#[61] "ThirdPartyPaymentTimes" "ThirdPartyPaymentUser"  "WagersCounts"           "WithdrawAmount"        
#[65] "WithdrawBet"            "WithdrawSums"           "WithdrawTimes"          "WithdrawUser"          
#[69] "SystemCode"            

dmm = CovMcd(reg7set[,c("AddictiveRate","balance","BetAmountSums","Bite",
                        "BitePerBet","BitePerUser", "CDepositBet" , "CDepositPTime",
                        "CommissionableBet","CommissionableSums", "CommissionableUser", "CreditDepositBet"   
                        )])
plot(dmm)

dmm = CovMcd(reg30set[,c("AddictiveRate","balance","BetAmountSums","Bite",
                        "BitePerBet","BitePerUser", "CDepositBet" , "CDepositPTime",
                        "CommissionableBet","CommissionableSums", "CommissionableUser", "CreditDepositBet"   
)])


