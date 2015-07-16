shinyServer(function(input, output) {
 XYs <- reactiveValues(
    Xlocs = NULL,
    Ylocs = NULL,
    vals= NULL
  )

  # Handle clicks on the plot
  observeEvent(input$plot_click, {
    if (is.null(XYs$Xlocs)) {
      # We don't have a first click, so this is the first click
      XYs$Xlocs <- input$plot_click$x
      XYs$Ylocs<-  input$plot_click$y
    } else {
    XYs$Xlocs<-append(input$plot_click$x,XYs$Xlocs)[1:min(8,(length(XYs$Xlocs)+1))]
    XYs$Ylocs<-append(input$plot_click$y,XYs$Ylocs)[1:min(8,(length(XYs$Ylocs)+1))] 
    }
    
      XYdat<-as.data.frame(cbind(X=XYs$Xlocs,Y=XYs$Ylocs))
      XYs$vals<-extract(stk,XYdat)
  })

   
 output$map1 <- renderPlot({
  #Plot the Map
      par(oma=c(0,0,0,0),mar=c(0,0,2,0),xpd=FALSE)
      
      plot(mapStk,1,maxpixels=60000,col=Colors,xaxt="n",yaxt="n",bty="n")

      XYdat<-as.data.frame(cbind(X=XYs$Xlocs,Y=XYs$Ylocs))
      if((any(!is.na(XYdat)))){
      points(x=XYdat$X,y=XYdat$Y,pch=21,col="black",bg=Cols[1:nrow(XYdat)],cex=2.5)
      }   
  })  
  
 output$map2 <- renderPlot({
  #Plot the Map
      par(oma=c(0,0,0,0),mar=c(0,0,2,0),xpd=FALSE)
      plot(mapStk,2,maxpixels=30000,col=Colors,xaxt="n",yaxt="n")

      XYdat<-as.data.frame(cbind(X=XYs$Xlocs,Y=XYs$Ylocs))
      if((any(!is.na(XYdat)))){
      points(x=XYdat$X,y=XYdat$Y,pch=21,col="black",bg=Cols[1:nrow(XYdat)],cex=2.5)
      }
  })
  
output$map3 <- renderPlot({
  #Plot the Map
      par(oma=c(0,0,0,0),mar=c(0,0,2,0),xpd=FALSE)
      plot(mapStk,3,maxpixels=30000,col=Colors,xaxt="n",yaxt="n")

      XYdat<-as.data.frame(cbind(X=XYs$Xlocs,Y=XYs$Ylocs))
      if((any(!is.na(XYdat)))){
      points(x=XYdat$X,y=XYdat$Y,pch=21,col="black",bg=Cols[1:nrow(XYdat)],cex=2.5)
      }
  })
 
 output$map4 <- renderPlot({
  #Plot the Map
      par(oma=c(0,0,0,0),mar=c(0,0,2,0),xpd=FALSE)
      plot(mapStk,4,maxpixels=30000,col=Colors,xaxt="n",yaxt="n")

      XYdat<-as.data.frame(cbind(X=XYs$Xlocs,Y=XYs$Ylocs))
      if((any(!is.na(XYdat)))){
      points(x=XYdat$X,y=XYdat$Y,pch=21,col="black",bg=Cols[1:nrow(XYdat)],cex=2.5)
      }
  })
        
output$curves1 <- renderPlot({
  #Plot the Curves
    response.curvesOneModel(fitLst[[1]],modelLst[[1]],XYs$vals)
  })

output$curves2 <- renderPlot({
  #Plot the Curves
    response.curvesOneModel(fitLst[[2]],modelLst[[2]],XYs$vals)
  })
    
output$curves3 <- renderPlot({
  #Plot the Curves
    response.curvesOneModel(fitLst[[3]],modelLst[[3]],XYs$vals)
  })
  
output$curves4 <- renderPlot({
  #Plot the Curves
    response.curvesOneModel(fitLst[[4]],modelLst[[4]],XYs$vals)
  })
  
output$interact<-renderPlot({
par(mfrow=c(2,2),mar=c(0,0,2,0),oma=c(0,0,0,0))
  response.curvesInteraction(fitLst[[1]],modelLst[[1]],vals,phi=input$phi,theta=input$theta)
  response.curvesInteraction(fitLst[[2]],modelLst[[2]],vals,phi=input$phi,theta=input$theta)
  response.curvesInteraction(fitLst[[3]],modelLst[[3]],vals,phi=input$phi,theta=input$theta)
  response.curvesInteraction(fitLst[[4]],modelLst[[4]],vals,phi=input$phi,theta=input$theta)
})
      
 
  #output$info <- renderPrint({
    # With base graphics, need to tell it what the x and y variables are.
   # nearPoints(ras, input$plot_click,threshold=500,addDist=TRUE,maxpoints=1)
     
    # nearPoints() also works with hover and dblclick events
  #})
})