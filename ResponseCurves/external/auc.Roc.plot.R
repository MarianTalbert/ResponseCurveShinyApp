auc.roc.plot<-function (DATA,Thresh, threshold = 101, find.auc = TRUE,model.names,col, 
                        na.rm = FALSE, xlab = "1-Specificity (false positives)",
    ylab = "Sensitivity (true positives)",TestTrain,
    cexMult=1.5){
  main=ifelse(TestTrain==1,"Calibration ROC Curves","Evaluation ROC Curves")
  N.dat <- ncol(DATA) - 2
  df <- data.frame()
  AUC<-vector()
  ThreshPoints<-as.data.frame(matrix(nrow=N.dat,ncol=4))
  Lab=vector()
    for (dat in 1:N.dat) {
        Model.dat <- roc.plot.calculate(DATA = DATA, threshold = threshold,
            which.model = dat)
        Model.dat$specificity<-1-Model.dat$specificity
        AUC[dat]<-round(auc(DATA,which.model=dat)$AUC,2)
        Lab[dat]<-paste(model.names[dat]," (AUC = ",AUC[dat],")",sep="")
        ThreshInd<-which.min((Model.dat$threshold-Thresh[[dat]])^2)
        ThreshPoints[dat,]<-c(Lab[dat],Thresh[[dat]],Model.dat$sensitivity[ThreshInd],
                              Model.dat$specificity[ThreshInd])
        if(dat==1){PlotDat<-cbind(Model.dat,Model=rep(Lab[dat],times=nrow(Model.dat)))
        }else{
          PlotDat<-rbind(PlotDat,cbind(Model.dat,Model=rep(Lab[dat],times=nrow(Model.dat))))
        }
    }
    
    PlotDat$Model<-factor(PlotDat$Model,levels=Lab,ordered=TRUE)
     
    colnames(ThreshPoints)<-c("Model","Thresh","sensitivity","specificity")
    ThreshPoints$Model<-factor(ThreshPoints$Model,levels=Lab,ordered=TRUE)
    ThreshPoints$sensitivity<-as.numeric(ThreshPoints$sensitivity)
    ThreshPoints$specificity<-as.numeric(ThreshPoints$specificity)
    p<-ggplot(PlotDat,aes(x=specificity,y=sensitivity,colour=Model)) + 
      geom_path(size=1) + xlim(0, 1) + ylim(0, 1)+xlab(xlab)+ylab(ylab)+
      scale_colour_manual(values=col)+
      geom_point(data=ThreshPoints,aes(x=specificity,y=sensitivity,colour=Model),size=rel(3))+
      theme(axis.title = element_text(size = rel(1.3)))+
      ggtitle(main)
return(p)
}
