interactiveMap <- function(predictedStk,binaryStk,messRast,Colors,Cols,input,i,boundary,Coords,Stats,XYs,
                           resp,Ensemble=FALSE,TestTrain){
  
  PresCoords<-Coords[resp==1,]
  AbsCoords<-Coords[resp==0,]  
  #Plot the Map
    if(input$showResid){
      Legend=FALSE 
      alpha=.2
      
    }else{
      Legend=TRUE 
      alpha=1
      
    }

    par(oma=c(0,0,0,0),mar=c(0,0,2,0),xpd=FALSE) 
    
    if(input$mapType=="mprob") plot(predictedStk,i,maxpixels=60000,breaks=pretty(c(min(minValue(predictedStk)),max(maxValue(predictedStk))),13),
                                    col=rev(inferno(12,begin=0,end=1,alpha=.8)),xaxt="n",yaxt="n",bty="n",legend=Legend,
                                    alpha=alpha,cex.main=1.7,xlim=XYs$xlim,ylim=XYs$ylim)
    if(input$mapType=="mbinary") plot(binaryStk,i,maxpixels=60000,xaxt="n",yaxt="n",bty="n",legend=Legend,
                                      alpha=alpha,col=rev(magma(12,begin=.4,end=1,alpha=.8)),
                                      cex.main=1.7,xlim=XYs$xlim,ylim=XYs$ylim)
    if(input$mapType=="mess"){ plot(messRast,maxpixels=60000,col=colorRampPalette(c("purple4","white","darkgreen"))(12),
                                   breaks=pretty(c(-100,100),13),xaxt="n",yaxt="n",bty="n",legend=Legend,alpha=alpha,
                                   cex.main=1.7,main="Multivariate Environmental Similary Surface",xlim=XYs$xlim,ylim=XYs$ylim)
    }
    if(class(boundary)=="SpatialPolygonsDataFrame") plot(boundary,add=TRUE)
    if(input$showResid & !Ensemble) residImage(x=Coords[,1],y=Coords[,2],z=Stats[[i]][[TestTrain]]$devResid,boundary,predictedStk,i,rastColors=Colors)
    XYdat <- as.data.frame(cbind(X=XYs$Xlocs,Y=XYs$Ylocs))
    if(!is.null(input$showTrain)){
      
      if("showPres"%in%input$showTrain) points(x=PresCoords[,1],y=PresCoords[,2],pch=21,col="red",bg="red4")
      if("showAbs"%in%input$showTrain) points(x=AbsCoords[,1],y=AbsCoords[,2],pch=21,col="dodgerblue",bg="blue4")
    } 
    if((any(!is.na(XYdat)))){
      
      points(x=XYdat$X,y=XYdat$Y,pch=21,col="black",bg=Cols[1:nrow(XYdat)],cex=2.5)  
    }
}