switchEvalPlots<-function(PlotType,Stats,modelNames,predictedVals,resp,varImp,Thresh,datNames,cexMult){
#add calibration plot once it's fixed
  
  if(PlotType == "EvaluationMetrics"){
    Statistics = unlist(lapply(Stats,FUN=function(x){x[c(1,3,4,5,6)]}))
    StatsFrame = data.frame(Stat=as.factor(names(Statistics)),
                            Value=as.vector(Statistics),
                            Model=as.factor(rep(modelNames,each=5)))
    
    g<-ggplot(StatsFrame,aes(x=Stat,y=Value,fill=Model,facets=Stat), color=factor(Model)) +
      stat_summary(fun.y=mean,position=position_dodge(),geom="bar")+scale_fill_brewer(palette="Set3")+
      ggtitle("Model Evaluation Metrics")+xlab("")
  
  }
  if(PlotType == "ROC"){
      pre<-cbind(seq(1:length(resp)),resp,matrix(unlist(predictedVals),ncol=length(modelNames)))
      g<-auc.roc.plot(pre,Thresh,col=c("red","blue","green","purple"),opt.thresholds=TRUE,
         opt.methods=2,model.names=modelNames,legend.cex=1.4,opt.legend.cex = 1.4)+
         theme(axis.text.x = element_text(size = rel(cexMult)))
      
  }

  if(PlotType == "ConfusionMatrix"){
    for(i in 1:length(modelNames)){
        M<-data.frame(Percent=c(100*Stats[[i]]$Cmx[2]/sum(Stats[[i]]$Cmx[1:2]),
                                100*Stats[[i]]$Cmx[4]/sum(Stats[[i]]$Cmx[3:4]),
                                100*Stats[[i]]$Cmx[1]/sum(Stats[[i]]$Cmx[1:2]),
                                100*Stats[[i]]$Cmx[3]/sum(Stats[[i]]$Cmx[3:4])),
                      Predicted=factor(c("Absence","Absence","Presence","Presence")),
                      Observed=factor(c("Presence","Absence","Presence","Absence")),
                      Model=rep(modelNames[i],times=4))
        
        if(i==1) Mdat <- M
        else Mdat<-rbind(Mdat,M)
    }
    
    BlueScale=brewer.pal(9,"Blues")
    g<-ggplot(Mdat,aes(x=Observed,y=Predicted))+geom_tile()+geom_tile(aes(fill=Percent))+
      scale_fill_gradient2(low="white",mid=BlueScale[4],high=BlueScale[9],midpoint=50,limits=c(0,100))+
      ggtitle("Confusion Matrix")+
      facet_wrap(~ Model) +
      geom_text(data=Mdat[seq(from=1,to=nrow(Mdat),by=4),], aes(x=2, y=1, label=paste(round(Percent),"%")))+
      geom_text(data=Mdat[seq(from=2,to=nrow(Mdat),by=4),], aes(x=1, y=1, label=paste(round(Percent),"%")))+
      geom_text(data=Mdat[seq(from=3,to=nrow(Mdat),by=4),], aes(x=2, y=2, label=paste(round(Percent),"%")))+
      geom_text(data=Mdat[seq(from=4,to=nrow(Mdat),by=4),], aes(x=1, y=2, label=paste(round(Percent),"%")))+
      theme(strip.text.x = element_text(size = rel(cexMult)))+
      theme(axis.text.x = element_text(size = rel(cexMult)))
 
  }
  if(PlotType=="VariableImportance"){
   
    #I'M ASSUMING FOR NOW CONSISTENT VARIABLES ACROSS MODELS
    varImpMat<-data.frame(VariableImportance=unlist(varImp),
                          Variable=as.factor(rep(datNames,times=length(modelNames))),
                          Model=as.factor(rep(modelNames,each=length(datNames))))
    
    g<-ggplot(varImpMat,aes(x=Variable,y=VariableImportance,fill=Model), color=Model) +  
      stat_summary(fun.y=mean,position=position_dodge(),geom="bar")+scale_fill_brewer(palette="Set3")+
        theme(axis.text.x = element_text(size = rel(cexMult)))+ylab("Drop in AUC")+xlab("Predictors")+
        ggtitle("Permutation Variable Importance")
  }
  if(PlotType=="Density"){
    densityFrame<-data.frame(Predicted=unlist(predictedVals),
                             Response=rep(resp,times=length(modelNames)),
                             Model=rep(modelNames,each=length(resp)))
    densityFrame$Model<-as.factor(densityFrame$Model)
    densityFrame$Response<-as.factor(densityFrame$Response)
    
    g<-ggplot(densityFrame,aes(x=Predicted,colour=Response,fill=Response))+geom_density(alpha=0.3)+
        facet_wrap(~ Model)+
        scale_fill_manual(values=c("blue","red"))+
        scale_colour_manual(values=c("blue","red"))+
        theme(strip.text.x = element_text(size = rel(cexMult)))+
        theme(axis.text.x = element_text(size = rel(cexMult)))
  
    }
    g<-g+theme(axis.title = element_text(size = rel(1.3))) +
      #theme(axis.title = element_text(size = rel(cexMult))) +	
      theme(plot.title =element_text(size=1.2*rel(cexMult)))+
      theme(legend.title=element_text(size=rel(cexMult)))+
      theme(legend.text=element_text(size=.9*rel(cexMult)))
      #theme(axis.text.y = element_text(size = rel(cexMult)))   
  
return(g)
}

switchEvalText<-function(PlotType){
  if(PlotType=="EvaluationMetrics")
      txt <- c("This plot shows evaluation metrics for the training data for all models.",
      "Eventually we hope to add Cross-Validation options, and the ability to click on points",
      "on the plot to obtaion aditional information ")
  if(PlotType=="ROC")
      txt <- c("The Area under the Reciver Operating Characteristic Cuves",
      "tells us how well we can descrimiate between a randomly chosen",
      "presence and a randomly chosen absence point",
      "values of .5 indicate that the model has no ability to discrimiate",
      "while 1 indicates perfect discrimiation.")
  if(PlotType=="VariableImportance")
      txt <- c("Variable importance is determined by calculating the drop in AUC when each variable",
      "is in turn randomly permuted.  A large drop in AUC would indicated an important predictor",
      "while if the AUC remains unchanged then the variable was less important in the model")
  if(PlotType=="Density")
      txt <- c("The density plot shows the distribution of the predictions split by response type",
      "a large overlap between these distributions would indicate that the model",
      "cannot discrimate well between the two groups")
  if(PlotType=="ConfusionMatrix") 
    txt<-c("The confusion matrix shows which observations were classified correctly and incorrectly",
           "by a model using the threshold selected to convert the continuous probabilities into a",
           "binary classifier")
  return(txt)
}

