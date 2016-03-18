formatModels <- function(fitLst,inputLayers,trainData,threshold,testData){
          
  #putting together all of the global input needed by both the server and ui fcts
  predictedStk<-varImp<-predictedVals<-binaryVals<-varIncluded<-Thresh<-binaryStk<-Stats<-cmxPlot<-Coords<-list()
  resp<-dat<-AUCVal<-list()
  #putting t
  if(inherits(fitLst,"list")){
    Biomd<-FALSE
    modelLst<-names(fitLst)
    Coords$train<-trainData[,c(1,2)]
    resp$train<-trainData[,3]
    dat$train<-trainData[,-c(1:3)]
    if(!missing(testData)){ 
      Coords$test<-testData[,c(1,2)]
      dat$test<-testData[,-c(1:3)]
      resp$test<-testData[,3]
    }
    
    Variables<-names(dat[[1]])
   
  } 
  
  if(inherits(fitLst,"BIOMOD.models.out")){
   
    Biomd<-TRUE
    modelLst<-fitLst@models.computed
    resp$train<-trainData@data.species
    Coords<-trainData@coord
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
    traindat<-trainData@data.env.var
    Variables<-names(traindat)
    
    myBiomodProjection <- BIOMOD_Projection(modeling.output = fitLst,
                                            new.env = inputLayers,
                                            proj.name = 'current',
                                            selected.models = 'all',
                                            compress = FALSE,
                                            build.clamping.mask = FALSE,keep.in.memory=TRUE,on_0_1000=FALSE)
    predictedStk <- myBiomodProjection@proj@val 
  }

  for(i in 1:length(modelLst)){
      cat(paste("preparing model", i,"of",length(modelLst),"\n")) 
      predictedVals[[i]]<-Thresh[[i]]<-Stats[[i]]<-varImp[[i]]<-list()
      
      for(split in 1:(ifelse(missing(testData),1,2))){
      
              if(!Biomd){ 
                 predictedVals[[i]][[split]]<-predictBinary(fitLst[[i]],
                                                            newdata=dat[[split]])
                 AUCVal[[split]]<-roc(resp[[split]],predictedVals[[i]][[split]])
              }else AUCVal[[split]]<- roc(resp[[split]],predictedVals[[i]][[split]])
           
            Thresh[[i]]<-optimal.thresholds(DATA=cbind(seq(1:nrow(dat[[1]])),
                                                       resp[[1]],predictedVals[[i]][[1]]))[2,threshold]
            binaryVals<-as.numeric(predictedVals[[i]][[split]]>=Thresh[[i]])
            
            Stats[[i]][[split]]<-calcStat(predictedVals[[i]][[split]],resp[[split]],Thresh[[i]])
            if(!Biomd){
              Permuted<-permutePredict(dat[[split]],fitLst[[i]],resp[[split]])
              if(split==1) varIncluded[[i]]<-Permuted$varIncluded  
              varImp[[i]][[split]]<-AUCVal[[split]]-Permuted$AUC
            }
      }
      if(i==1){ 
        #all rasters calculated with training data and training thresholds
        if(!Biomd) predictedStk<-predict(model=fitLst[[i]],object=inputLayers,fun=predictBinary)
        binaryStk<-createBinary(predictedStk[[i]][[1]],Thresh[[i]])
        messRast<-mess(inputLayers,dat[[1]],full=FALSE) 
      }else{ 
        if(!Biomd) predictedStk<-addLayer(predictedStk,predict(object=inputLayers,model=fitLst[[i]],
                                                               fun=predictBinary))
        binaryStk<-addLayer(binaryStk,createBinary(predictedStk[[i]][[1]],Thresh[[i]]))
      }
  }

  EnsemblePred<-stackApply(predictedStk,indices=rep(1,times=length(fitLst)),fun=mean,na.rm=FALSE)
  EnsembleBin<-stackApply(binaryStk,indices=rep(1,times=length(fitLst)),fun=sum,na.rm=FALSE)
  predictedStk<-addLayer(predictedStk,EnsemblePred)
  binaryStk<-addLayer(binaryStk,EnsembleBin)
  
  names(predictedStk)<-c(modelLst,"Ensemble_Mean_of_Probability_Maps")
  names(binaryStk)<-c(modelLst,"Ensemble_Sum_of_Binary_Maps")
 
  #return just the range of the calibration data
  d=data.frame(Name=names(dat[[1]]),min=apply(dat[[1]],2,min,na.rm=TRUE),
               max <-apply(dat[[1]],2,max,na.rm=TRUE),mean=apply(dat[[1]],2,mean,na.rm=TRUE))
  dataLst <- split(d,f=seq(1:nrow(d)))
  returnLst<-list(binaryStk=binaryStk,Coords=Coords,dat=dat,
                  dataLst=dataLst,EnsembleBin=EnsembleBin,
                  EnsemblePred=EnsemblePred,messRast=messRast,
                  modelLst=modelLst,predictedStk=predictedStk,predictedVals=predictedVals,
                  resp=resp,Stats=Stats,Thresh=Thresh,
                  varIncluded=varIncluded,varImp=varImp,Variables=Variables)
  return(returnLst)
}

