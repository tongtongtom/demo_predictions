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
 print(to_select)
 RawData = datasets[[to_select]]
 print(head(RawData))
 print("Dataset:")
 if (grepl('stand',lookuptable[[DatasetName]],fixed = TRUE))
 {
   lpn = paste( gsub('stand','',lookuptable[[DatasetName]]),'normdays',sep='')
   RawDataNonStand = datasets[[lpn]]
   indxs = filtermin(RawDataNonStand, Minusers,AvgUsers)

   if (grepl('(mean|sd)',Aggregations,perl = TRUE))
   {
       print('filtered')
       RawData = RawData[0,]
       relevant_Dataset = relevant_Dataset[0,]
       return(list('relevant_data' = relevant_Dataset,
                   'RawData' = RawData))
   } 
 } else {
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
   return(list('relevant_data' = relevant_Dataset[relevant_Dataset$SystemCode %in% indxs,]
              ,'RawData' = RawData[RawData$SystemCode %in% indxs,]))
} 
 
filterout = function(ip){
  return(ip[!(ip %in% c('SystemCode','DATE','sequence')) ])
}
