library(caret)
library(MASS)
library(mlbench)
library(rpart)

TrainData <- sdmdata[,4:7]
TrainClasses <- factor(sdmdata[,3])
createFolds(factor(sdmdata[,3]), 
                    k=5)
set.seed(1)
tmp <- createDataPartition(sdmdata$pb,
                           p = .8,
                           list = FALSE)
training <- sdmdata[ tmp,]
testing <- sdmdata[-tmp,]

knnFit1 <- train(TrainData[tmp,], TrainClasses[tmp],
                 method = "knn",
                 preProcess = c("center", "scale"),
                 tuneLength = 10,
                 trControl = trainControl(method = "cv"))
knnFit2 <- train(TrainData[tmp,], TrainClasses[tmp],
                 method = "knn",
                 preProcess = c("center", "scale"),
                 tuneLength = 10,
                 trControl = trainControl(method = "boot"))

nnetFit <- train(TrainData[tmp,], TrainClasses[tmp],
                 method = "nnet",
                 preProcess = "range",
                 tuneLength = 2,
                 trace = FALSE,
                 maxit = 100)

glmFit <- train(TrainData[tmp,], TrainClasses[tmp],
               method = "glm")

earthFit <- train(TrainData[tmp,], TrainClasses[tmp],
                  method = "bagEarthGCV")
#Still working out cv folding make sure it works with the gui and maybe try to plot evaluation 
#metrics for best fit
#maybe ask the creator if I can get mean probability vectors from hold out
# folds=5
# repeats=1
# myControl <- trainControl(method='cv', number=folds, repeats=repeats, 
#                           returnResamp='none', classProbs=TRUE,
#                           returnData=FALSE, savePredictions=TRUE, 
#                           verboseIter=TRUE, allowParallel=TRUE,
#                           summaryFunction=twoClassSummary,
#                           index=createMultiFolds(TrainClasses, k=folds, times=repeats))

rpartFit <- train(TrainData[tmp,], TrainClasses[tmp],
                  method = "glm",
                    metric = "ROC",trControl=myControl)

rdaFit<-train(TrainData[tmp,],TrainClasses[tmp],method="rda")
GLM_Model = glm(pb ~ bio1 + bio5 + bio12+ bio7, data=sdmdata[tmp,],family=binomial)
MARS_Model = earth(pb~ bio1 + bio5 + bio12 + bio7, data=sdmdata[tmp,],glm=list(family=binomial))

fitLst<-list(nneFit=nnetFit,knnFit=knnFit1,glmFit=glmFit,earthFit=earthFit,
             MARS_Model=MARS_Model,glmFit=glmFit)

fitLst2<-list(GLM_Model=GLM_Model,MARS_Model=MARS_Model,glmFit=glmFit,earthFit=earthFit,
              nneFit=nnetFit,knnFit=knnFit1)

exploreCurves(fitLst,inputLayers=layerStk,trainData=sdmdata,threshold=2,boundary=wrld_simpl)
exploreCurves(fitLst2,inputLayers=layerStk,trainData=sdmdata[tmp,],threshold=2,
              boundary=wrld_simpl,testData=sdmdata[-tmp,])
exploreCurves(fitLst,inputLayers=layerStk,trainData=sdmdata,threshold=2,boundary=wrld_simpl)

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

#or load pre-calculated results using:
  #load(url("http://caret.r-forge.r-project.org/exampleModels.RData"))
  
  resamps <- resamples(list(CART = rpartFit,
                            CondInfTree = ctreeFit,
                            MARS = earthFit))

resamps
summary(resamps)

