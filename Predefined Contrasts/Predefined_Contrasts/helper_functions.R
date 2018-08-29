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
            FUN = function(x)c(min = min(x),max = max(x),mean = mean(x),median = median(x),sd = sd(x))
            )
 print(dim(summaryset))
#print(head(summaryset))
#print(head(summaryset[,2]))
#print((6*c(1:sum(subsetter)) - 1)+1)
#
}


estimate_probability = function(target)
{
  k=3
  resultaat = 0
  x = 3.01 ##needs to be bigger than 3 for approximation to be valid
  while( resultaat >¡@target)
  {
    x = x*1.01 
    resultaat = 3/2 * log((x/k * exp(1 - (x/k))))
  }
  return(x) ##resultaat is a multiplier on the scale
}


filtermin = function(datam, mincut, avgcut)
{
  print(head(datam))
  mindata = aggregate(datam[,c('MemberCounts','WagersCounts')],by=list(datam$SystemCode), min)
  avgdata = aggregate(datam[,c('MemberCounts','WagersCounts')],by=list(datam$SystemCode), mean)
  listone = mindata[ mindata$MemberCounts <= mincut, 'Group.1'] 
  listtwo = avgdata[ avgdata$MemberCounts <= avgcut, 'Group.1'] 
  #print( datam$SystemCode[ ! (datam$SystemCode %in% unique(c(listone,listtwo)))] )
  #print( unique(datam$SystemCode[ ! (datam$SystemCode %in% unique(c(listone,listtwo)))]) )
  return( unique(datam$SystemCode[ ! (datam$SystemCode %in% unique(c(listone,listtwo)))]))

}
