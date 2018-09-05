# Insert the right number of plot output objects into the web page
#output$Harbour <- renderUI({
#  plot_output_list <- lapply(1:length(input$characteristics), function(i) {
#    plotname <- paste("Harbour", i, sep="")
#    print("Reading:")
#    print(plotname)
#    plotOutput(plotname, height = 500, width = 750)
#  })
#  
#  # Convert the list to a tagList - this is necessary for the list of items
#  # to display properly.
#  do.call(tagList, plot_output_list)
#})

# Call renderPlot for each one. Plots are only actually generated when they
# are visible on the web page.



create_summary = function(ds, sel)
{
 summaryset = aggregate(x = ds, 
            by = sel,  
            FUN = function(x)c(min = min(abs(x)),max = max(abs(x)),median = median(x),mean = mean(x),sd = sd(x))
            )
 functionlist = c('min','max','median','mean','sd')
 
 referencedf= data.frame()
 template = c("SystemCode",colnames(ds),'summary')
 print(template)
 print("Iterations:")
 for (k in c(1:length(functionlist)))
 {
   first_col = as.data.frame(summaryset[,1])
   for (l in c(2:(length(unique(colnames(ds)))+1)))
   {
     extractpart = summaryset[,l][,k]
     first_col = cbind(first_col,extractpart)
   }
   first_col$summary = functionlist[k]
   referencedf = rbind(referencedf,first_col)
 }
 #print(head(referencedf))
 #print(template)
 
 colnames(referencedf) = template
 print("Leaving Helper Function:")
 return(referencedf)
}


estimate_probability = function(target)
{
  k=3
  resultaat = 0
  x = 3.01 ##needs to be bigger than 3 for approximation to be valid
  while( resultaat > target)
  {
    x = x*1.01 
    resultaat = 3/2 * log((x/k * exp(1 - (x/k))))
  }
  return(x) ##resultaat is a multiplier on the scale
}

filterout = function(ip){
  return(ip[!(ip %in% c('SystemCode','DATE','sequence')) ])
}

filtermin = function(datam, mincut, avgcut)
{
  mindata = aggregate(datam[,c('MemberCounts','WagersCounts')],by=list(datam$SystemCode), min)
  avgdata = aggregate(datam[,c('MemberCounts','WagersCounts')],by=list(datam$SystemCode), mean)
  listone = mindata[ mindata$MemberCounts <= mincut, 'Group.1'] 
  listtwo = avgdata[ avgdata$MemberCounts <= avgcut, 'Group.1'] 
  #print( datam$SystemCode[ ! (datam$SystemCode %in% unique(c(listone,listtwo)))] )
  #print( unique(datam$SystemCode[ ! (datam$SystemCode %in% unique(c(listone,listtwo)))]) )
  return( unique(datam$SystemCode[ ! (datam$SystemCode %in% unique(c(listone,listtwo)))]))

}
