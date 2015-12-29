formatModels <- function(fitLst,inputLayers,data,threshold,...){
          
  #putting together all of the global input needed by both the server and ui fcts
  predictedStk<-varImp<-predictedVals<-binaryVals<-varIncluded<-Thresh<-binaryStk<-Stats<-cmxPlot<-list()
 
  #putting t
  if(inherits(fitLst,"list")){
    Biomd<-FALSE
    modelLst<-names(fitLst)
    PresCoords<-data[data[,3]==1,c(1,2)]
    AbsCoords<-data[data[,3]==0,c(1,2)]
    resp<-data[,3]
    dat<-data[,-c(1:3)]
    Variables<-names(dat)
    Split<-seq(1:length(resp))
  } 
  
  if(inherits(fitLst,"BIOMOD.models.out")){
   
    Biomd<-TRUE
    modelLst<-fitLst@models.computed
    resp<-data@data.species
    PresCoords<-data@coord[resp==1,]
    AbsCoords<-data@coord[resp==0,]
    preds<-get_predictions(fitLst,as.data.frame=TRUE)
    if(any(preds>2)) preds<-preds/1000 #biomod converts to a 0 to 1000 scale
    #evalPreds<-get_predictions(myBiomodModelOut,as.data.frame=TRUE,evaluation=TRUE)
    #should be able to look at evaluation data as well
    #predicted values identically 1 or 0 cause problems in devianc resids
   
    preds[which(preds==1,arr.ind=TRUE)] <- .999999999
    preds[which(preds==0,arr.ind=TRUE)] <- .00000001
    predictedVals<-as.list(preds)
    vi<-as.data.frame(get_variables_importance(myBiomodModelOut))
    varImp<-as.list(vi)
    varIncluded<-as.list(as.data.frame(vi>0))
    browser()
    dat<-data@data.env.var
    Variables<-names(dat)
    Split<-seq(1:length(resp))
    myBiomodProjection <- BIOMOD_Projection(modeling.output = fitLst,
                                            new.env = inputLayers,
                                            proj.name = 'current',
                                            selected.models = 'all',
                                            compress = FALSE,
                                            build.clamping.mask = FALSE,keep.in.memory=TRUE,on_0_1000=FALSE)
    predictedStk <- myBiomodProjection@proj@val 
  }
 
  for(i in 1:length(modelLst)){
    if(!Biomd){ predictedVals[[i]]<-predictBinary(fitLst[[i]],dat)
    if(inherits(fitLst[[i]],"randomForest")){
      #because of the distinction between oob prediction and in bag prediction
      #we end up with two sets of predicted vals for rf 
      inBagResp<-predict(fitLst[[i]],dat,type='response')
      AUCVal<-roc(resp,inBagResp)
    }else{
      AUCVal<-roc(resp,predictedVals[[i]])
    }
    }else AUCVal<- roc(resp,predictedVals[[i]])
    Thresh[[i]]<-optimal.thresholds(DATA=cbind(seq(1:nrow(dat)),resp,predictedVals[[i]]))[2,threshold]
    binaryVals<-as.numeric(predictedVals[[i]]>=Thresh[[i]])
    
    Stats[[i]]<-calcStat(predictedVals[[i]],resp,Split,Thresh[[i]])
    if(i==1){ 
      if(!Biomd) predictedStk<-predict(inputLayers,fitLst[[i]],type='response')
      binaryStk<-createBinary(predictedStk[[i]],Thresh[[i]])
      messRast<-mess(inputLayers,dat,full=FALSE) 
    }else{ 
      if(!Biomd) predictedStk<-addLayer(predictedStk,predict(inputLayers,fitLst[[i]],type='response'))
      binaryStk<-addLayer(binaryStk,createBinary(predictedStk[[i]],Thresh[[i]]))
    }
    
    if(!Biomd){
      Permuted<-permutePredict(dat,fitLst[[i]],resp)
      varIncluded[[i]]<-Permuted$varIncluded  
      varImp[[i]]<-AUCVal-Permuted$AUC}
    M<-data.frame(Percent=c(100*Stats[[i]]$Cmx[2]/sum(Stats[[i]]$Cmx[1:2]),100*Stats[[i]]$Cmx[4]/sum(Stats[[i]]$Cmx[3:4]),
                            100*Stats[[i]]$Cmx[1]/sum(Stats[[i]]$Cmx[1:2]),100*Stats[[i]]$Cmx[3]/sum(Stats[[i]]$Cmx[3:4])),
                  Predicted=factor(c("Absence","Absence","Presence","Presence")),
                  Observed=factor(c("Presence","Absence","Presence","Absence")),Model=rep(modelLst[i],times=4))
    
    if(i==1) Mdat <- M
    else Mdat<-rbind(Mdat,M)
  }
  
  densityFrame<-data.frame(Predicted=unlist(predictedVals),Response=rep(resp,times=length(fitLst)),
                           Model=rep(modelLst,each=length(resp)))
  densityFrame$Model<-as.factor(densityFrame$Model)
  densityFrame$Response<-as.factor(densityFrame$Response)
  
  
  
  
  EnsemblePred<-stackApply(predictedStk,indices=rep(1,times=length(fitLst)),fun=mean)
  EnsembleBin<-stackApply(binaryStk,indices=rep(1,times=length(fitLst)),fun=sum)
  predictedStk<-addLayer(predictedStk,EnsemblePred)
  binaryStk<-addLayer(binaryStk,EnsembleBin)
  
  names(predictedStk)<-c(modelLst,"Ensemble_Mean_of_Probability_Maps")
  names(binaryStk)<-c(modelLst,"Ensemble_Sum_of_Binary_Maps")
  
  pre<-cbind(seq(1:length(resp)),resp,matrix(unlist(predictedVals),ncol=length(modelLst)))
  
  #I'M ASSUMING FOR NOW CONSISTENT VARIABLES ACROSS MODELS
  varImpMat<-data.frame(VariableImportance=unlist(varImp),
                        Variable=as.factor(rep(names(dat),times=length(modelLst))),
                        Model=as.factor(rep(modelLst,each=length(Variables))))
  Statistics = unlist(lapply(Stats,FUN=function(x){x[c(1,3,4,5,6)]}))
  StatsFrame = data.frame(Stat=as.factor(names(Statistics)),Value=as.vector(Statistics),Model=as.factor(rep(modelLst,each=5)))
  
  
  d=data.frame(Name=names(dat),min=apply(dat,2,min,na.rm=TRUE),
               max <-apply(dat,2,max,na.rm=TRUE),mean=apply(dat,2,mean,na.rm=TRUE))
  dataLst <- split(d,f=seq(1:nrow(d)))
  returnLst<-list(AbsCoords=AbsCoords,binaryStk=binaryStk,dat=dat,densityFrame=densityFrame,
                  dataLst=dataLst,EnsembleBin=EnsembleBin,
                  EnsemblePred=EnsemblePred,Mdat=Mdat,messRast=messRast,pre=pre,
                  modelLst=modelLst,predictedStk=predictedStk,predictedVals=predictedVals,
                  PresCoords=PresCoords,resp=resp,Stats=Stats,StatsFrame=StatsFrame,Thresh=Thresh,varIncluded=varIncluded,
                  varImp=varImp,
                  varImpMat=varImpMat,
                  Variables=Variables)
  return(returnLst)
}

