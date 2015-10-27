confusionMatrix<-function(Stats,split.type){

     #par(oma=c(4,3,5,3),mar=c(20,6,5,2))
  if(split.type=="none") lo<-layout(matrix(data=c(1,2), nrow=1, ncol=2), c(4.5,1), 1)
   else {lo<-layout(matrix(data=c(1,2,3), nrow=1, ncol=3), c(4.5,4.5,1), 1)
         
       if(split.type=="crossValidation"){
                                 a<-lapply(Stats[names(Stats)!="train"],function(lst){lst$Cmx})
                                  cmx<-a[[1]]
                                  for(i in 2:length(a)) cmx<-cmx+a[[i]]
                                  csv.stats<-apply(do.call("rbind",(lapply(Stats[names(Stats)!="train"],function(lst){
                                       return(c(lst$Sens,lst$Specf,lst$Pcc,lst$Kappa,lst$Tss))}))),2,mean)
                                Stats$crossValidation<-list(Cmx=cmx,Sens=csv.stats[1],Specf=csv.stats[2],Pcc=csv.stats[3],Kappa=csv.stats[4],Tss=csv.stats[5])
                                Stats<-list("crossValidation"=Stats$crossValidation,"train"=Stats$train)
            }
          }
 
#zlim<-c(min(unlist(lapply(Stats,function(lst){100*lst$Cmx/sum(lst$Cmx)}))),max(unlist(lapply(Stats,function(lst){100*lst$Cmx/sum(lst$Cmx)}))))
#instead of basing the zlim on the acutal confusion matricies, base them on the maximum achievable value for a cell given the ratio of pres/abs
       extract.max<-function(lst){
         max(100*table(lst$auc.data$pres.abs)/length(lst$auc.dat$pres.abs))
         }
         options(warn=-1)
zlim=c(0,100)
        options(warn=0)
          Main="Confusion matrix"
  for(i in length(Stats):1){
      image((1:2),c(2,4),matrix(data=c(100*Stats[[i]]$Cmx[2]/sum(Stats[[i]]$Cmx[1:2]),100*Stats[[i]]$Cmx[4]/sum(Stats[[i]]$Cmx[3:4]),
                                       100*Stats[[i]]$Cmx[1]/sum(Stats[[i]]$Cmx[1:2]),100*Stats[[i]]$Cmx[3]/sum(Stats[[i]]$Cmx[3:4])),nrow=2),
               zlim=zlim,xaxt="n",yaxt="n",xlab="",
               ylab="",main=Main,col=heat.colors(100)[100:1],cex.lab=2,cex.main=2.5)
          mtext("Absence",side=2,at=2,cex=2,lwd=1.3)
          mtext("Presence",side=2,at=4,cex=2,lwd=1.3)
          mtext("Presence",side=1,at=1,cex=2,line=1,lwd=1.3)
          mtext("Absence",side=1,at=2,cex=2,line=1,lwd=1.3)
          text(x=c(1,1,2,2),y=c(2,4,2,4),
          labels=c(Stats[[i]]$Cmx[2],Stats[[i]]$Cmx[1],
                   Stats[[i]]$Cmx[4],
                   Stats[[i]]$Cmx[3]),cex=5)
              abline(h=3,lwd=5)
              abline(v=1.5,lwd=5)
        box()
    }
  mtext("Observed",1,outer=TRUE,lwd=2,cex=2.5)
  mtext("Predicted",2,outer=TRUE,lwd=2,cex=2.5)
  
### color scale
 image(1,seq(from=zlim[1],to=zlim[2],length=50),
               matrix(data=seq(from=zlim[1],to=zlim[2],length=50), ncol=50,nrow=1),
              col=heat.colors(50)[50:1],
              xlab="",ylab="",zlim=zlim,
              xaxt="n",cex.lab=2,cex.axis=2)

}

