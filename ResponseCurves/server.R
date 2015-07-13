shinyServer(function(input, output) {

XYupdate <- reactive({
if(!is.null(input$plot_click)){
browser()
  Xlocs<-append(input$plot_click$x,Xlocs)[1:min(5,(length(Xlocs)+1))]
  Ylocs<-append(input$plot_click$y,Ylocs)[1:min(5,(length(Ylocs)+1))]
  }else{
   Xlocs<-NA
   Ylocs<-NA
  }
  XYdat<-as.data.frame(cbind(X=Xlocs,Y=Ylocs))
  return(XYdat)
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
      xy<-XYupdate()
      browser()
      if((any(!is.na(xy)))){
      xy$group<-as.factor(seq(1:nrow(xy)))
      g<-g+geom_point(data=xy, aes(x = X, y = Y,group=group),fill=Cols[nrow(xy)],colour=rep("black",times=nrow(xy)),size=7,shape=21)
      g
      }
     g
     
  })  
  
output$curves <- renderPlot({
  #Plot the Curves
 XYs<-XYupdate()
 if(any(!is.na(XYs))) vals<-extract(stk,XYs)
 else vals<-NULL
    response.curves(fitLst,modelLst,vals)
  })
  

  
  
 
  #output$info <- renderPrint({
    # With base graphics, need to tell it what the x and y variables are.
   # nearPoints(ras, input$plot_click,threshold=500,addDist=TRUE,maxpoints=1)
     
    # nearPoints() also works with hover and dblclick events
  #})
})