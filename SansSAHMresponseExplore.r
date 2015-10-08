library(maptools)
library(randomForest)
library(mgcv)
library(dismo)
library(shiny)
library(earth)
library(nnet)
library(kernlab)
 files <- list.files(path=paste(system.file(package="dismo"),
 '/ex', sep=''), pattern='grd', full.names=TRUE)
 files<-files[c(1,5,2,7)]
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
 backgr <- randomPoints(layerStk, 500)
 absvals <- extract(layerStk, backgr)
 pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
 sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))
 sdmdata[,"biome"] = as.factor(sdmdata[,"biome"])
 head(sdmdata)

fitLst<-list()
predictedStk<-list()
varImpLst<-list()
fitLst<-list(
 GLM_Model = glm(pb ~ bio1 + bio5 + bio12+ bio7, data=sdmdata,family=binomial),
 MARS_Model = earth(pb~ bio1 + bio5 + bio12 + bio7, data=sdmdata,glm=list(family=binomial)),
 RF_Model=randomForest(pb~ bio1 + bio5 + bio12 + bio7,data=sdmdata),
 GAM_Model<-gam(pb ~ s(bio1) + s(bio5) + s(bio12) + s(bio7), data=sdmdata),
 SVM_Model <- ksvm(pb ~ bio1+bio5+bio12+bio7, data=sdmdata)
 )


 
modelLst<-names(fitLst)
for(i in 1:length(fitLst)){
predictedStk[[i]]<-predict(layerStk,fitLst[[i]],type='response')
varImpLst[[i]]<-runif(1:4) #obviously replace eventurally
}
predictedStk<-stack(predictedStk)
names(predictedStk)<-names(fitLst)
Cols<<-c(wes_palette("Darjeeling"),wes_palette("GrandBudapest2"),wes_palette("Cavalcanti"),wes_palette("Moonrise3"))
max_plots<-5



dat<<-sdmdata[,-1]
Variables<<-names(dat)
resp<<-sdmdata[,1]
 d=data.frame(Name=names(dat),min=apply(dat,2,min,na.rm=TRUE),
   max=apply(dat,2,max,na.rm=TRUE),mean=apply(dat,2,mean,na.rm=TRUE))
dataLst<<-split(d,f=seq(1:nrow(d)))
IntractVals<-vector()
rspHgt<-c("150px","300px","550px","750px")[length(fitLst)]

#=========================================
#    This is where the magic happens
runApp("C:\\GoogleDrive\\Interactive\\Rcode\\Shiny\\MyCode\\ResponseCurves")
runApp("C:\\GoogleDrive\\Interactive\\Rcode\\Shiny\\MyCode\\PairsExplore")


r2<-sampleRegular(r,size=20000,xy=TRUE)
a<- matrix(r2[,3],nrow=length(unique(r2[,1])))
a<-a[,ncol(a):1]
rastTm<-Sys.time()
plot(r,maxpixels=100000,col=Colors)
Sys.time()-rastTm

rastTm<-Sys.time()
my.filled.contour(a, plot.axes = {},col=Colors,nlevels = 26)
Sys.time()-rastTm

vals<-rbind(c(.2,.1,50,-10,.2,.06,.5,2),
c(.9,-.6,50,-10,.2,.06,.5,2),
c(.2,.1,50,-10,.2,0,.17,2),
c(.2,.1,50,-10,.2,.06,.5,0))

vals<-rbind(c(0.683187430, 0.683187421, 0.69550804, 0.3144333),
            c(0.001984252, 0.001984253, 0.03192426, 0.0035000),
            c(0.948119431, 0.948119426, 0.96974946, 0.6888333))


responseCurves(f=list(fitLst[[1]]),m=list(modelLst[[2]]),varImp=list(varImpLst[[2]]),addImp=F,vals,dat=dat,resp=resp)
interactionPlot(fitLst[[1]],modelLst[[1]],vals=NULL,theta=30,phi=25,x="bio1",y="bio5",dat=dat,resp=resp)
densityPlot(fitLst[[3]])

