library(caret)
library(MASS)
library(mlbench)
library(rpart)

TrainData <- sdmdata[,4:7]
TrainClasses <- factor(sdmdata[,3])
createFolds(factor(sdmdata[,3]), 
                    k=5)
set.seed(1)
inTrain<-createDataPartition()
knnFit1 <- train(TrainData, TrainClasses,
                 method = "knn",
                 preProcess = c("center", "scale"),
                 tuneLength = 10,
                 trControl = trainControl(method = "cv"))
knnFit2 <- train(TrainData, TrainClasses,
                 method = "knn",
                 preProcess = c("center", "scale"),
                 tuneLength = 10,
                 trControl = trainControl(method = "boot"))

nnetFit <- train(TrainData, TrainClasses,
                 method = "nnet",
                 preProcess = "range",
                 tuneLength = 2,
                 trace = FALSE,
                 maxit = 100)

glmFit <- train(TrainData, TrainClasses,
               method = "glm")

earthFit <- train(TrainData, TrainClasses,
                  method = "bagEarthGCV")

tmp <- createDataPartition(TrainClasses,
                           p = .8,
                           times = 10)
folds=5
repeats=1
myControl <- trainControl(method='cv', number=folds, repeats=repeats, 
                          returnResamp='none', classProbs=TRUE,
                          returnData=FALSE, savePredictions=TRUE, 
                          verboseIter=TRUE, allowParallel=TRUE,
                          summaryFunction=twoClassSummary,
                          index=createMultiFolds(TrainClasses, k=folds, times=repeats))

rpartFit <- train(TrainData, TrainClasses,
                  method = "glm",
                    metric = "ROC",trControl=myControl)

rdaFit<-train(TrainData,TrainClasses,method="rda")
GLM_Model = glm(pb ~ bio1 + bio5 + bio12+ bio7, data=sdmdata,family=binomial)
MARS_Model = earth(pb~ bio1 + bio5 + bio12 + bio7, data=sdmdata,glm=list(family=binomial))
fitLst<-list(nneFit=nnetFit,knnFit=knnFit1,glmFit=glmFit,earthFit=earthFit,
             MARS_Model=MARS_Model,glmFit=glmFit)

fitLst2<-list(GLM_Model=GLM_Model,MARS_Model=MARS_Model,glmFit=glmFit,earthFit=earthFit,
              nneFit=nnetFit,knnFit=knnFit1)

exploreCurves(fitLst,inputLayers=layerStk,data=sdmdata,threshold=2,boundary=wrld_simpl)
exploreCurves(fitLst2,inputLayers=layerStk,data=sdmdata,threshold=2,boundary=wrld_simpl)


#this resampling might be the best way to get at the evaluation metrics
data(BloodBrain)
set.seed(1)

tmp <- createDataPartition(logBBB,
                           p = .8,
                           times = 10)

rpartFit <- train(bbbDescr, logBBB,
                  "rpart", 
                  tuneLength = 16,
                  trControl = trainControl(
                    method = "LGOCV", index = tmp))

ctreeFit <- train(bbbDescr, logBBB,
                  "ctree", 
                  trControl = trainControl(
                    method = "LGOCV", index = tmp))

earthFit <- train(bbbDescr, logBBB,
                  "earth",
                  tuneLength = 20,
                  trControl = trainControl(
                    method = "LGOCV", index = tmp))

or load pre-calculated results using:
  #load(url("http://caret.r-forge.r-project.org/exampleModels.RData"))
  
  resamps <- resamples(list(CART = rpartFit,
                            CondInfTree = ctreeFit,
                            MARS = earthFit))

resamps
summary(resamps)

