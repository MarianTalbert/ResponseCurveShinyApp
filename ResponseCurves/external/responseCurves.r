responseCurves<-function(fitLst,model,vals=NULL,varImp,varIncluded,addImp,pIdx,dat,resp,Cols,Ensemble,
                         mapType="none",modelIdx=NA,TestTrain){

  biomd=inherits(fitLst,"BIOMOD.models.out") 
       
         mins  <- sapply(dat, min,  na.rm=TRUE)
         maxs  <- sapply(dat, max,  na.rm=TRUE)
         means <- sapply(dat, mean, na.rm=TRUE)
         n <- 100

          bgRamp<-colorRampPalette(c("white","grey75"))(10)
          Colors<-c("black",Cols)
        if(is.null(vals)) vals<-matrix(means,nrow=1)
         else vals<-rbind(means,vals)

         byVar<-ifelse(missing(pIdx),FALSE,TRUE)
         if(!byVar & !biomd) fitLst <- list(f=fitLst[[modelIdx]])


            if(byVar) par(mfrow=c((length(model)+1),1),mar=c(0,2,0,0),oma=c(3,1,0,0),xpd=TRUE)
            else { par(mfrow=c(1,ncol(dat)),mar=c(0,0,3,0),oma=c(0,5,0,0),xpd=TRUE)
                  if(ncol(dat)>9) par(mfrow=c(2,ceiling(ncol(dat)/2)))
            }

              y.lim=c(0,1)
              nRow<-1
             #set up what we're cycling over
              modelCycle<-1:length(model)
              predCycle<-1:ncol(dat)
              if(Ensemble) {
                RespLst<-list() #list is a list of predictors
                RespMat<-matrix(ncol=nrow(vals),nrow=n)
              }
              if(byVar) predCycle <- pIdx

               for(j in modelCycle){
                 
                    allVarImp<-rep(0,times=ncol(dat))
                    if(!byVar) allVarImp<-unlist(varImp)
                    if(byVar) allVarImp<-as.vector(varImp[[j]][[TestTrain]])
                    allVarImp[allVarImp<0]<-0 #set the minimum to zero so it shows up white
                    bgCol<-bgRamp[cut(x=allVarImp,breaks=seq(from=0,to=max(allVarImp),length=11),include.lowest=TRUE)]
                 for (pIdx in predCycle) {
                     plotR<- varIncluded[[j]][pIdx] #I'm not sure this information is always available

                     for(v in 1:nrow(vals)){
                              test <- do.call("rbind", replicate(n, vals[v,], simplify=FALSE))
                              test[,pIdx] <- seq(mins[pIdx], maxs[pIdx], length.out=n)
                              test<-as.data.frame(test)
                              test<-rbind(test,vals[v,])
                              colnames(test)<-names(means)
                              if(biomd) Response<-predictBinary(model=fitLst, newdata=test,model[[j]])
                              else Response<-predictBinary(model=fitLst[[j]], newdata=test)
                              
                               lR<-length(Response)
                               if(Ensemble) RespMat[,v]<-Response[1:(lR-1)]

                                if(v==1 & !Ensemble){

                                   plot(test[1:(lR-1),pIdx],Response[1:(lR-1)], ylim = y.lim, xlab = "",
                                    type = ifelse(plotR,"l","n"), lwd=2,cex=3,cex.main=3,cex.axis=1.2,yaxt=ifelse(pIdx==1 & !byVar,"s","n"),
                                    ylab=ifelse(pIdx==1 & !byVar,"Predicted Value",""),
                                    xaxt="n",main="",bg=bgCol[v])
                                    if(addImp){
                                      Xext<-extendrange(test[1:(lR-1),pIdx])
                                      Yext<-extendrange(c(0,1))
                                      rect(Xext[1],Yext[1],Xext[2],Yext[2],col=bgCol[pIdx])
                                    }

                                    if(!plotR) box(col="grey82")
                                 }
                                 if(plotR & !Ensemble){
                                    lines(test[1:(lR-1),pIdx],Response[1:(lR-1)], ylim = y.lim, xlab = "",
                                      ylab = "", type = "l", lwd=2,cex=3,cex.main=3,cex.axis=1.2,yaxt=ifelse(!byVar,"s","n"),
                                      xaxt="n",main="",col=Colors[v])
                                      segments(x0=vals[v,pIdx],y0=0,y1=Response[lR],x1=vals[v,pIdx],col=Colors[v],lty=2,cex=2)
                                      points(x=vals[v,pIdx],y=Response[lR],col=Colors[v],pch=16,cex=2)
                                 }
                                 Names<-names(dat)
                                 Names<-paste(substr(Names,start=1,stop=12),c("\n","")[1+(nchar(Names)<=12)],substr(Names,start=13,stop=nchar(Names)),sep="")
                               if(byVar & pIdx==1) mtext(model[[j]],line=1,side=2,cex=1.5)
                               if(!byVar) mtext(Names[pIdx],line=1,side=3,cex=1.7,col=ifelse(plotR,"black","grey74"))
                               # if(pIdx==1) mtext(model,side=2,outer=TRUE,at=seq(from=1/(2*nRow),to=(1-1/(2*nRow)),length=nRow)[j+1],line=3,cex=1.2)
                     }
                     if(Ensemble) RespLst[[pIdx]]<-RespMat
                    }
               }
             
              if(mapType=="mess") mtext(model,side=2,cex=1.7,outer=TRUE,line=2)
               if(byVar){
                    presDens<-density(dat[resp==1,pIdx])
                    absDens<-density(dat[resp==0,pIdx])
                    plot(x=c(mins[pIdx],maxs[pIdx]),y=c(0,max(absDens$y,presDens$y)),type="n",
                    ylab="",xlab=names(dat)[pIdx],yaxt="n",bty="n",cex.axis=1.5)
                    polygon(absDens,col=changeAlpha("blue",alpha=.5),border="blue")
                    polygon(presDens,col=changeAlpha("red",alpha=.5),border="red")
                    for(v in 1:nrow(vals)){
                      segments(x0=vals[v,pIdx],y0=0,y1=25,x1=vals[v,pIdx],col=Cols[v-1],lty=2,cex=2)
                    }
                    if(pIdx==1) mtext("Density",side=2,cex=1.2)
               }
              if(Ensemble) return(RespLst)
  }






















