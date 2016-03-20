ensemebleCurves <- function(fitLst,modelLst,dat,Cols,XYs,varIncluded,varImp,mapType="none",TestTrain){
    respLst<-list()
   
    for(i in 1:length(fitLst)){
      respLst[[i]]<-responseCurves(fitLst,list(m=modelLst[[i]]),vals=XYs$vals,
                                   varIncluded=list(varIncluded[[i]]),varImp=list(varImp[[i]][[TestTrain]]),addImp=FALSE,
                                   dat=dat,resp=resp,Cols=Cols,Ensemble=TRUE,modelIdx=i)
    }
    Cols  <- c("black",Cols) 
    mins  <- sapply(dat, min,  na.rm=TRUE)
    maxs  <- sapply(dat, max,  na.rm=TRUE)
    means <- sapply(dat, mean, na.rm=TRUE)
    
    par(mfrow=c(1,ncol(dat)),mar=c(0,0,3,0),oma=c(0,5,0,0),xpd=TRUE)
    
    for(v in 1:length(respLst[[1]])){ #working over the variables within each model
      varfromEachModel<-lapply(respLst,"[",v)
      respSum <-varfromEachModel[[1]][[1]]
      for(j in 2:length(varfromEachModel)) respSum <- respSum+unlist(varfromEachModel[[j]][[1]])
      respSum<-respSum/length(fitLst)
      plot(c(mins[v],maxs[v]),c(0,1),type="n",yaxt=ifelse(v==1,"s","n"))
      mtext(names(dat)[v],line=1,side=3,cex=1.7)
      pts<-1
      for(pts in 1:ncol(respSum))
         lines(seq(mins[v],maxs[v],length=nrow(respLst[[1]][[1]])),
            respSum[,pts],col=Cols[pts],lwd=2,cex=3,
            cex.main=3,cex.axis=1.2)
      #add points to the curves...
      #segments(x0=vals[v,pIdx],y0=0,y1=Response[lR],x1=vals[v,pIdx],col=Colors[v],lty=2,cex=2)
      #points(x=vals[v,pIdx],y=Response[lR],col=Colors[v],pch=16,cex=2)
    
    }
    if(mapType=="mess") mtext("Ensemble",side=2,cex=1.7,outer=TRUE,line=2)
}