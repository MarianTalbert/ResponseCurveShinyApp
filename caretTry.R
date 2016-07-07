setwd("C:\\GoogleDrive\\Interactive\\Rcode\\Shiny\\MyCode")
ShinyCode<-file.path("ResponseCurves\\External")
sourceList<-list.files(ShinyCode,full.names=TRUE)
unlist(lapply(as.list(sourceList),source))

ChkLibs(list("rgeos","maptools","randomForest","mgcv","dismo","shiny","earth","PresenceAbsence",
             "wesanderson","ggplot2","raster","grid","gridExtra","splines","RColorBrewer",
             "viridis","caret","MASS","mlbench","rpart"))
#=====================================================
# This is almost directly from the dismo vignette 
files <- list.files(path=paste(system.file(package="dismo"),
                               '/ex', sep=''), pattern='grd', full.names=TRUE)
files<-files[c(1,3,4,5,2,7,8)]
files<-files[-c(9)]
layerStk <- stack(files)
plot(layerStk)
data(wrld_simpl)
file <- paste(system.file(package="dismo"), "/ex/bradypus.csv", sep="")
bradypus <- read.table(file, header=TRUE, sep=",")
# we do not need the first column
bradypus <- bradypus[,-1]
#And now plot:
# first layer of the RasterStack
plot(layerStk, 1)
# note the "add=TRUE" argument with plot
plot(wrld_simpl, add=TRUE)
# with the points function, "add" is implicit
points(bradypus, col="blue")
presvals <- extract(layerStk, bradypus)
# setting random seed to always create the same
# random set of points for this example
set.seed(0)
#setting up an extent around the presence points
PresExt <- extent(-104.7,-36,-26,16)
backgr <- randomPoints(layerStk,ext=PresExt, 500)
colnames(backgr)<-c("lon","lat")
absvals <- extract(layerStk, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(rbind(bradypus,backgr),cbind(pb, rbind(presvals, absvals)))


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

