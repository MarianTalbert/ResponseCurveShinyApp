interactiveMap<-function(predictedStk,binaryStk,messRast,Colors,Cols,input,i,boundary,data,Stats,XYs,
                         PresCoords,AbseCoords,Ensemble=FALSE){
    #Plot the Map
    if(input$showResid){
      Legend=FALSE 
      alpha=.2
      
    }else{
      Legend=TRUE 
      alpha=1
      
    }

    Titlep<-ifelse(Ensemble,"Ensemble (Mean) of Probability Maps",names(predictedStk)[i])
    Titleb<-ifelse(Ensemble,"Ensemble (Sum) of Binary Maps",names(binaryStk)[i])
    par(oma=c(0,0,0,0),mar=c(0,0,2,0),xpd=FALSE) 
    
    if(input$mapType=="mprob") plot(predictedStk,i,maxpixels=60000,col=Colors,xaxt="n",yaxt="n",bty="n",legend=Legend,
                                    alpha=alpha,main=Titlep)
    if(input$mapType=="mbinary") plot(binaryStk,i,maxpixels=60000,xaxt="n",yaxt="n",bty="n",legend=Legend,
                                      alpha=alpha,main=Titleb)
    if(input$mapType=="mess") plot(messRast,maxpixels=60000,col=colorRampPalette(c("magenta","white","green"))(21),
                                   breaks=pretty(c(-100,100),22),xaxt="n",yaxt="n",bty="n",legend=Legend,alpha=alpha,
                                   main="Multivariate Environmental Similarity Surface")
    if(class(boundary)=="SpatialPolygonsDataFrame") plot(boundary,add=TRUE)
    if(input$showResid & !Ensemble) residImage(x=data$lon,y=data$lat,z=Stats[[i]]$devResid,boundary,predictedStk,i,rastColors=Colors)
    XYdat<-as.data.frame(cbind(X=XYs$Xlocs,Y=XYs$Ylocs))
    if(!is.null(input$showTrain)){
      
      if("showPres"%in%input$showTrain) points(x=PresCoords[,1],y=PresCoords[,2],pch=21,col="white",bg="red")
      if("showAbs"%in%input$showTrain) points(x=AbsCoords[,1],y=AbsCoords[,2],pch=21,col="white",bg="blue")
    } 
    if((any(!is.na(XYdat)))){
      
      points(x=XYdat$X,y=XYdat$Y,pch=21,col="black",bg=Cols[1:nrow(XYdat)],cex=2.5)  
    }
}