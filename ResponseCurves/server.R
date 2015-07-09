shinyServer(function(input, output) {
  output$map <- renderPlot({
  #Plot the Map
    ggplot(data=ras, aes(y=Latitude, x=Longitude)) +
      geom_raster(aes(fill=MAP)) +
      theme_bw() +
      coord_equal() +
      scale_fill_gradientn (
            colours=Colors,
            values = c (seq(0,1,length.out=255)))+
      #scale_fill_gradient('MAP (mm/yr)", limits=c(0,2500)) +
      theme(axis.title.x = element_text(size=16),
      axis.title.y = element_text(size=16, angle=90),
      axis.text.x = element_text(size=14),
      axis.text.y = element_text(size=14),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      legend.key = element_blank()
      )
  })
  
  
  output$curves <- renderPlot({
  #Plot the Curves
  print(input$plot_click)
  vals<-NULL
  if(!is.null(input$plot_click)){
  x <<- append(x, input$plot_click$x)
  y <<- append(y, input$plot_click$y)      
  vals<-extract(stk,cbind(x,y))
  }
  
    response.curves(fitLst,modelLst,vals)
  })
  output$info <- renderPrint({
    # With base graphics, need to tell it what the x and y variables are.
    nearPoints(ras, input$plot_click,threshold=500,addDist=TRUE,maxpoints=1)
     
    # nearPoints() also works with hover and dblclick events
  })
})