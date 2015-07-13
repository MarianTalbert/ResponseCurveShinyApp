shinyServer(function(input, output) {
 XYs <- reactiveValues(
    Xlocs = NULL,
    Ylocs = NULL
  )

  # Handle clicks on the plot
  observeEvent(input$plot_click, {
    if (is.null(XYs$Xlocs)) {
      # We don't have a first click, so this is the first click
      XYs$Xlocs <- input$plot_click$x
      XYs$Ylocs<-  input$plot_click$y
    } else {
    XYs$Xlocs<-append(input$plot_click$x,XYs$Xlocs)[1:min(5,(length(XYs$Xlocs)+1))]
    XYs$Ylocs<-append(input$plot_click$y,XYs$Ylocs)[1:min(5,(length(XYs$Ylocs)+1))]

    }
  })

output$map <- renderPlot({
  #Plot the Map
      par(oma=c(0,0,0,0))
      plot(map,col=Colors,xaxt="n",yaxt="n")

      XYdat<-as.data.frame(cbind(X=XYs$Xlocs,Y=XYs$Ylocs))
      if((any(!is.na(XYdat)))){
      points(x=XYdat$X,y=XYdat$Y,pch=21,col="black",bg=Cols[1:nrow(XYdat)],cex=2.5)
      }

     
  })  
  
output$curves <- renderPlot({
  #Plot the Curves

 XYdat<-as.data.frame(cbind(X=XYs$Xlocs,Y=XYs$Ylocs))
 if(any(!is.null(XYs$Xlocs))) vals<-extract(stk,XYdat)
 else vals<-NULL
    response.curvesOneModel(fitLst[[1]],modelLst[[1]],vals)
  })
  

  
  
 
  #output$info <- renderPrint({
    # With base graphics, need to tell it what the x and y variables are.
   # nearPoints(ras, input$plot_click,threshold=500,addDist=TRUE,maxpoints=1)
     
    # nearPoints() also works with hover and dblclick events
  #})
})