library(maptools)
library(randomForest)
library(mgcv)
 files <- list.files(path=paste(system.file(package="dismo"),
 '/ex', sep=''), pattern='grd', full.names=TRUE )
 predictors <- stack(files)
  plot(predictors)
 data(wrld_simpl)
 file <- paste(system.file(package="dismo"), "/ex/bradypus.csv", sep="")
 bradypus <- read.table(file, header=TRUE, sep=",")
 # we do not need the first column
 bradypus <- bradypus[,-1]
#And now plot:
 # first layer of the RasterStack
 plot(predictors, 1)
 # note the "add=TRUE" argument with plot
 plot(wrld_simpl, add=TRUE)
 # with the points function, "add" is implicit
 points(bradypus, col="blue")
presvals <- extract(predictors, bradypus)
 # setting random seed to always create the same
 # random set of points for this example
 set.seed(0)
 backgr <- randomPoints(predictors, 500)
 absvals <- extract(predictors, backgr)
 pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
 sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))
 sdmdata[,"biome"] = as.factor(sdmdata[,"biome"])
 head(sdmdata)

fitLst<-list()
mapStk<-list()
fitLst<-list(
 GAM_Model = gam(pb ~ bio1 + bio5 + bio12, data=sdmdata,family=binomial),
 GLM_Model = glm(pb ~ bio1 + bio5 + bio12, data=sdmdata,family=binomial),
 MARS_Model = earth(pb~ bio1 + bio5 + bio12, data=sdmdata,glm=list(family=binomial)),
 RF_Model=randomForest(pb~ bio1 + bio5 + bio12,data=sdmdata)
 )

for(i in 1:length(fitLst)){
mapStk[[i]]<-predict(predictors,fitLst[[i]])
}
mapStk<-stack(mapStk)

Cols<<-c(wes_palette("Darjeeling"),wes_palette("GrandBudapest2"),wes_palette("Cavalcanti"),wes_palette("Moonrise3"))
max_plots<-5

Variables<<-names(sdmdata)

dat<<-sdmdata[,-1]
resp<<-sdmdata[,1]
 d=data.frame(Name=names(dat),min=apply(dat,2,min,na.rm=TRUE),
   max=apply(dat,2,max,na.rm=TRUE),mean=apply(dat,2,mean,na.rm=TRUE))
dataLst<<-split(d,f=seq(1:nrow(d)))
IntractVals<-vector()
rspHgt<-c("150px","300px","550px","750px")[length(fitLst)]

#=========================================
#    This is where the magic happens
runApp("C:\\GoogleDrive\\Python\\DevWorkspace\\userpackages\\sahm\\pySAHM\\Resources\\R_Modules\\ResponseCurves")
