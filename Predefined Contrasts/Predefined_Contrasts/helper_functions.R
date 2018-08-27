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
