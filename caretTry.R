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
                  
RandForest_Model <- train(TrainData[tmp,], TrainClasses[tmp],
                  method = "rf",nodesize=30)

rda_Model<-train(TrainData[tmp,],TrainClasses[tmp],method="rda")
GLM_Model = glm(pb ~ bio1 + bio5 + bio12+ bio7, data=sdmdata[tmp,],family=binomial)
MARS_Model = earth(pb~ bio1 + bio5 + bio12 + bio7, data=sdmdata[tmp,],glm=list(family=binomial))

fitLst<-list(GLM=GLM_Model,MARS=MARS_Model,RandForest=RandForest_Model,nnet=nnetFit)


correlationViewer(sdmdata,layerStk)
exploreCurves(fitLst,inputLayers=layerStk,trainData=sdmdata[tmp,],threshold=2,
              boundary=wrld_simpl,testData=sdmdata[-tmp,])

















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

