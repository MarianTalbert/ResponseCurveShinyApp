switchEvalPlots<-function(PlotType,StatsFrame,pre,Thresh,Mdat,varImpMat,densityFrame,modelNames,cexMult){
#add calibration plot once it's fixed
  
  if(PlotType == "EvaluationMetrics")
g<-ggplot(StatsFrame,aes(x=Stat,y=Value,fill=Model,facets=Stat), color=factor(Model)) +
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar")+scale_fill_brewer(palette="Blues")+
  ggtitle("Model Evaluation Metrics")+xlab("")
  

if(PlotType == "ROC")
g<-auc.roc.plot(pre,Thresh,col=c("red","blue","green","purple"),opt.thresholds=TRUE,
             opt.methods=2,model.names=modelNames,legend.cex=1.4,opt.legend.cex = 1.4)+
    theme(axis.text.x = element_text(size = rel(cexMult)))

if(PlotType == "ConfusionMatrix")
g<-ggplot(Mdat,aes(x=Observed,y=Predicted))+geom_tile()+geom_tile(aes(fill=Percent))+
  scale_fill_gradient2(low="white",mid="yellow",high="red3",midpoint=50,limits=c(0,100))+
    ggtitle("Confusion Matrix")+
     facet_wrap(~ Model) +
      geom_text(data=Mdat[seq(from=1,to=nrow(Mdat),by=4),], aes(x=2, y=1, label=paste(round(Percent),"%")))+
      geom_text(data=Mdat[seq(from=2,to=nrow(Mdat),by=4),], aes(x=1, y=1, label=paste(round(Percent),"%")))+
      geom_text(data=Mdat[seq(from=3,to=nrow(Mdat),by=4),], aes(x=2, y=2, label=paste(round(Percent),"%")))+
      geom_text(data=Mdat[seq(from=4,to=nrow(Mdat),by=4),], aes(x=1, y=2, label=paste(round(Percent),"%")))+
      theme(strip.text.x = element_text(size = rel(cexMult)))+
      theme(axis.text.x = element_text(size = rel(cexMult)))

if(PlotType=="VariableImportance")
g<-ggplot(varImpMat,aes(x=Variable,y=VariableImportance,fill=Model), color=Model) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar")+scale_fill_brewer(palette="Blues")+
    theme(axis.text.x = element_text(size = rel(cexMult)))+ylab("Drop in AUC")+xlab("Predictors")
    ggtitle("Permutation Variable Importance")
  
if(PlotType=="Density")
g<-ggplot(densityFrame,aes(x=Predicted,colour=Response,fill=Response))+geom_density(alpha=0.3)+
    facet_wrap(~ Model)+
    scale_fill_manual(values=c("blue","red"))+
    scale_colour_manual(values=c("blue","red"))+
    theme(strip.text.x = element_text(size = rel(cexMult)))+
    theme(axis.text.x = element_text(size = rel(cexMult)))

g<-g+theme(axis.text.y = element_text(size = rel(cexMult))) +
  theme(axis.title = element_text(size = rel(cexMult))) +	
  theme(plot.title =element_text(size=1.2*rel(cexMult)))+
  theme(legend.title=element_text(size=rel(cexMult)))+
  theme(legend.text=element_text(size=.9*rel(cexMult)))+
  theme(axis.text.y = element_text(size = rel(cexMult)))   
 
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
      "while if the AUC remains unchanged then the variable was les important in the model")
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

