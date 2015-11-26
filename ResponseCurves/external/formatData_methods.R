if (!isGeneric("formatModels")) {
  setGeneric("formatModels", function(fitLst,...){
    standardGeneric("formatModels")
  })
}

setMethod("formatModels","ANY", function(x,...){
            #x is fit list here
            for(i in 1:length(modelLst)){
              AUCVal<- roc(resp,predictedVals[[i]])
              Thresh[[i]]<-optimal.thresholds(DATA=cbind(seq(1:nrow(dat)),resp,predictedVals[[i]]))[2,threshold]
              binaryVals<-as.numeric(predictedVals[[i]]>=Thresh[[i]])
              Stats[[i]]<-calcStat(predictedVals[[i]],resp,Split,Thresh[[i]])
              if(i==1){
                binaryStk<-createBinary(predictedStk[[i]],Thresh[[i]])
                messRast<-mess(inputLayers,dat,full=FALSE) 
              }else{ 
                binaryStk<-addLayer(binaryStk,createBinary(predictedStk[[i]],Thresh[[i]]))
              }
              
              M<-data.frame(Percent=c(100*Stats[[i]]$Cmx[2]/sum(Stats[[i]]$Cmx[1:2]),100*Stats[[i]]$Cmx[4]/sum(Stats[[i]]$Cmx[3:4]),
                                      100*Stats[[i]]$Cmx[1]/sum(Stats[[i]]$Cmx[1:2]),100*Stats[[i]]$Cmx[3]/sum(Stats[[i]]$Cmx[3:4])),
                            Predicted=factor(c("Absence","Absence","Presence","Presence")),
                            Observed=factor(c("Presence","Absence","Presence","Absence")),Model=rep(modelLst[i],times=4))
              
              if(i==1) Mdat <- M
              else Mdat<-rbind(Mdat,M)
            }
            
            densityFrame<-data.frame(Predicted=unlist(predictedVals),Response=rep(resp,times=length(modelLst)),
                                     Model=rep(names(fitLst),each=length(resp)))
            densityFrame$Model<-as.factor(densityFrame$Model)
            densityFrame$Response<-as.factor(densityFrame$Response)
            x@AnyMethod<-"inAny"
            
})

setMethod("formatModels","list",function(x,...){
           
#putting together all of the global input needed by both the server and ui fcts
  predictedStk<-varImp<-predictedVals<-binaryVals<-varIncluded<-Thresh<-binaryStk<-Stats<-cmxPlot<-list()
  
    Biomd<-FALSE
    modelLst<-names(x)
    PresCoords<-data[data[,3]==1,c(1,2)]
    AbsCoords<-data[data[,3]==0,c(1,2)]
    resp<-data[,3]
    dat<-data[,-c(1:3)]
    Variables<-names(dat)
    Split<-seq(1:length(resp))
 
  for(i in 1:length(modelLst)){
    predictedVals[[i]]<-predictBinary(x[[i]],dat)
    if(inherits(x[[i]],"randomForest")){
      #because of the distinction between oob prediction and in bag prediction
      #we end up with two sets of predicted vals for rf 
      inBagResp<-predict(x[[i]],dat,type='response')
      AUCVal<-roc(resp,inBagResp)
    }else{
      AUCVal<-roc(resp,predictedVals[[i]])
    }
    
    Thresh[[i]]<-optimal.thresholds(DATA=cbind(seq(1:nrow(dat)),resp,predictedVals[[i]]))[2,threshold]
    binaryVals<-as.numeric(predictedVals[[i]]>=Thresh[[i]])
    
    Stats[[i]]<-calcStat(predictedVals[[i]],resp,Split,Thresh[[i]])
    if(i==1){ 
      predictedStk<-predict(inputLayers,fitLst[[i]],type='response')
      binaryStk<-createBinary(predictedStk[[i]],Thresh[[i]])
      messRast<-mess(inputLayers,dat,full=FALSE) 
    }else{ 
      predictedStk<-addLayer(predictedStk,predict(inputLayers,fitLst[[i]],type='response'))
      binaryStk<-addLayer(binaryStk,createBinary(predictedStk[[i]],Thresh[[i]]))
    }
    
    
      Permuted<-permutePredict(dat,fitLst[[i]],resp)
      varIncluded[[i]]<-Permuted$varIncluded  
      varImp[[i]]<-AUCVal-Permuted$AUC
    M<-data.frame(Percent=c(100*Stats[[i]]$Cmx[2]/sum(Stats[[i]]$Cmx[1:2]),100*Stats[[i]]$Cmx[4]/sum(Stats[[i]]$Cmx[3:4]),
                            100*Stats[[i]]$Cmx[1]/sum(Stats[[i]]$Cmx[1:2]),100*Stats[[i]]$Cmx[3]/sum(Stats[[i]]$Cmx[3:4])),
                  Predicted=factor(c("Absence","Absence","Presence","Presence")),
                  Observed=factor(c("Presence","Absence","Presence","Absence")),Model=rep(names(x)[i],times=4))
    
    if(i==1) Mdat <- M
    else Mdat<-rbind(Mdat,M)
  }
  
  densityFrame<-data.frame(Predicted=unlist(predictedVals),Response=rep(resp,times=length(fitLst)),
                           Model=rep(names(fitLst),each=length(resp)))
  densityFrame$Model<-as.factor(densityFrame$Model)
  densityFrame$Response<-as.factor(densityFrame$Response)
  
  EnsemblePred<-stackApply(predictedStk,indices=rep(1,times=length(fitLst)),fun=mean)
  EnsembleBin<-stackApply(binaryStk,indices=rep(1,times=length(fitLst)),fun=sum)
  predictedStk<-addLayer(predictedStk,EnsemblePred)
  binaryStk<-addLayer(binaryStk,EnsembleBin)
  
  names(predictedStk)<-c(names(fitLst),"Ensemble_Mean_of_Probability_Maps")
  names(binaryStk)<-c(names(fitLst),"Ensemble_Sum_of_Binary_Maps")
  modelNames<-names(fitLst)
  x@listMethod<-"inList"
  callNextMethod()
})

setMethod("formatModels","BIOMOD.models.out",
          
          function(x,...){
            
              #putting together all of the global input needed by both the server and ui fcts
              predictedStk<-varImp<-predictedVals<-binaryVals<-varIncluded<-Thresh<-binaryStk<-Stats<-cmxPlot<-list()
            
                Biomd<-TRUE
                modelLst<-fitLst@models.computed
                resp<-data@data.species
                PresCoords<-data@coord[resp==1,]
                AbsCoords<-data@coord[resp==0,]
                preds<-get_predictions(fitLst,as.data.frame=TRUE)
                #evalPreds<-get_predictions(myBiomodModelOut,as.data.frame=TRUE,evaluation=TRUE)
                #should be able to look at evaluation data as well
                predictedVals<-as.list(preds)
                vi<-as.data.frame(get_variables_importance(myBiomodModelOut))
                varImp<-as.list(vi)
                varIncluded<-as.list(vi>0)
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
              
                #This isn't right for the biomod ensemble I"ll need to figure it out
              #EnsemblePred<-stackApply(predictedStk,indices=rep(1,times=length(fitLst)),fun=mean)
              #EnsembleBin<-stackApply(binaryStk,indices=rep(1,times=length(fitLst)),fun=sum)
              #predictedStk<-addLayer(predictedStk,EnsemblePred)
              #binaryStk<-addLayer(binaryStk,EnsembleBin)
              
              #names(predictedStk)<-c(names(fitLst),"Ensemble_Mean_of_Probability_Maps")
              #names(binaryStk)<-c(names(fitLst),"Ensemble_Sum_of_Binary_Maps")
              #modelNames<-names(fitLst)
            })