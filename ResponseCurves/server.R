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
    browser()
    XYs$Xlocs<-append(input$plot_click$x,XYs$Xlocs)[1:min(5,(length(XYs$Xlocs)+1))]
    XYs$Ylocs<-append(input$plot_click$y,XYs$Ylocs)[1:min(5,(length(XYs$Ylocs)+1))]

    }
  })

output$map <- renderPlot({
  #Plot the Map      
    g<-ggplot(data=ras, aes(y=Latitude, x=Longitude)) +
      geom_raster(aes(fill=MAP)) +
      #theme_bw() +
      coord_equal() +
      scale_fill_gradientn (
            colours=Colors,
            values = c (seq(0,1,length.out=255)))+
            #scale_x_discrete(breaks=NULL)+
            #scale_y_discrete(breaks=NULL)+
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

      XYdat<-as.data.frame(cbind(X=XYs$Xlocs,Y=XYs$Ylocs))
      if((any(!is.na(XYdat)))){
      XYdat$group<-as.factor(seq(1:nrow(XYdat)))
      g<-g+geom_point(data=XYdat, aes(x = X, y = Y,group=group),fill=Cols[1:nrow(XYdat)],colour=rep("black",times=nrow(XYdat)),size=7,shape=21)
      }
     g
     
  })  
  
output$curves <- renderPlot({
  #Plot the Curves

 XYdat<-as.data.frame(cbind(X=XYs$Xlocs,Y=XYs$Ylocs))
 if(any(!is.null(XYs$Xlocs))) vals<-extract(stk,XYdat)
 else vals<-NULL
    response.curves(fitLst,modelLst,vals)
  })
  

  
  
 
  #output$info <- renderPrint({
    # With base graphics, need to tell it what the x and y variables are.
   # nearPoints(ras, input$plot_click,threshold=500,addDist=TRUE,maxpoints=1)
     
    # nearPoints() also works with hover and dblclick events
  #})
})