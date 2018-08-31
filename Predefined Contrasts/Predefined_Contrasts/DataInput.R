readindata = function()
{
  datasets = list()
  datasets[['d7normreg']]   = read.table( file = "../Data/reg7dset" ,sep=';', stringsAsFactors = FALSE)
  datasets[['d7normdays']]  = read.table( file = "../Data/days7dset" ,sep=';',stringsAsFactors = FALSE)
  datasets[['d7standreg']]  = read.table(file = "../Data/reg7stand" ,sep=';', stringsAsFactors = FALSE)
  datasets[['d7standdays']] = read.table(file = "../Data/days7stand" ,sep=';', stringsAsFactors = FALSE)

  datasets[['d30normreg']]  = read.table( file = "../Data/reg30dset" ,sep=';',stringsAsFactors = FALSE)
  datasets[['d30normdays']] = read.table( file = "../Data/days30dset" ,sep=';',stringsAsFactors = FALSE)
  datasets[['d30standreg']] = read.table(file = "../Data/reg30stand" ,sep=';',stringsAsFactors = FALSE)
  datasets[['d30standdays']]= read.table(file = "../Data/days30stand" ,sep=';',stringsAsFactors = FALSE)

  datasets[['d90normreg']]  = read.table( file = "../Data/reg90dset" ,sep=';',stringsAsFactors = FALSE)
  datasets[['d90normdays']] = read.table( file = "../Data/days90dset" ,sep=';',stringsAsFactors = FALSE)
  datasets[['d90standreg']] = read.table(file = "../Data/reg90stand" ,sep=';',stringsAsFactors = FALSE)
  datasets[['d90standdays']]= read.table(file = "../Data/days90stand" ,sep=';',stringsAsFactors = FALSE)
  return(datasets)
}


getDataSet = function(DatasetName, Minusers,AvgUsers,Aggregations, datasets)
{
 aggregations = c('regression','min','max','median','mean','sd')
 lookuptable = list( '7days normalized' = 'd7stand',
                     '7 days' = 'd7norm',
                     '30 days normalized' = 'd30stand',
                     '30 days' ='d30norm'  ,
                     '90 days normalized' = 'd90stand',
                     '90 days' = 'd90norm'
                )
 to_select = paste( lookuptable[[DatasetName]] , 'days',sep='')
 RawData = datasets[[to_select]]
 print("Dataset:")
 if (grepl('stand',lookuptable[[DatasetName]],fixed = TRUE))
 {
   lpn = paste( gsub('stand','',lookuptable[[DatasetName]]),'normdays',sep='')
   RawDataNonStand = datasets[[lpn]]
   indxs = filtermin(RawDataNonStand, Minusers,AvgUsers)
   #print(Aggregations)
   if (grepl('(mean|sd)',Aggregations,perl = TRUE))
   {
       print('filtered')
       RawData = RawData[0,]
       relevant_Dataset = relevant_Dataset[0,]
       return(list('relevant_Dataset' = relevant_Dataset,
                   'RawData' = RawData))
   } 
 }else {
   indxs = filtermin(RawData, Minusers, AvgUsers)
 }
   
   
 if (Aggregations == 'regression'){ 
   to_select = paste(lookuptable[[DatasetName]], 'reg', sep='')
   relevant_Dataset = datasets[[to_select]]
   
 } else {
   #print(sort(filterout(colnames(RawData))))
   header = filterout(colnames(RawData))
   print("Prefiltered:")
   relevant_Dataset = create_summary(RawData[,header],list(RawData$SystemCode))

   relevant_Dataset = relevant_Dataset[ relevant_Dataset$summary == Aggregations, ]
   rownames(relevant_Dataset) = relevant_Dataset$SystemCode
 }
   return(list('relevant_Dataset' = relevant_Dataset[relevant_Dataset$SystemCode %in% indxs,]
              ,'RawData' = RawData[RawData$SystemCode %in% indxs,]))
} 
 
filterout = function(ip){
  return(ip[!(ip %in% c('SystemCode','DATE','sequence')) ])
}


# if( DataSetName == '7days normalized')
# { 
#  indxs = filtermin(d7standdays, input$MinUsers, input$AvgUsers)
#  relevant_Dataset <<- d7standreg[indxs, ] 
#  RawData <<- d7standdays[d7standdays$SystemCode %in% indxs, ]
# } else if ( DataSetName == '7 days')
#{
#  indxs = filtermin(d7normdays, input$MinUsers, input$AvgUsers)
#  relevant_Dataset <<- d7normreg[indxs,] 
#  RawData <<- d7normdays[ d7normdays$SystemCode %in% indxs, ]
#} else if( DataSetName == '30 days normalized')
#{
#  indxs = filtermin(d30standdays, input$MinUsers, input$AvgUsers)
#  relevant_Dataset <<- d30standreg[indxs,] 
#  RawData <<- d30standdays[ d30standdays$SystemCode %in% indxs,]
#} else if (DataSetName == '30 days')
#{
#  indxs = filtermin(d30normdays, input$MinUsers, input$AvgUsers)
#  relevant_Dataset <<- d30normreg[indxs,] 
#  RawData <<- d30normdays[ d30normdays$SystemCode %in% indxs,]
#} else if( DataSetName == '90 days normalized')
#{
#  indxs = filtermin(d90standdays, input$MinUsers, input$AvgUsers)
#  relevant_Dataset <<- d90standreg[indxs,] 
#  RawData <<- d90standdays[ d90standdays$SystemCode %in% indxs,]
#} else if (DataSetName == '90 days')
#{
#  indxs = filtermin(d90normdays, input$MinUsers, input$AvgUsers)
#  relevant_Dataset <<- d90normreg[indxs,] 
#  RawData <<- d90normdays[ d90normdays$SystemCode %in% indxs,]
#}
