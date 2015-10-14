library(maptools)
library(randomForest)
library(mgcv)
library(dismo)
library(shiny)
library(earth)
library(nnet)
library(kernlab)
library(PresenceAbsence)
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
 colnames(backgr)<-c("lon","lat")
 absvals <- extract(layerStk, backgr)
 pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
 sdmdata <- data.frame(rbind(bradypus,backgr),cbind(pb, rbind(presvals, absvals)))
 sdmdata[,"biome"] = as.factor(sdmdata[,"biome"])
 head(sdmdata)


fitLst<-list(
 GLM_Model = glm(pb ~ bio1 + bio5 + bio12+ bio7, data=sdmdata,family=binomial),
 MARS_Model = earth(pb~ bio1 + bio5 + bio12 + bio7, data=sdmdata,glm=list(family=binomial)),
 RF_Model=randomForest(pb~ bio1 + bio5 + bio12 + bio7,data=sdmdata),
 GAM_Model<-gam(pb ~ s(bio1) + s(bio5) + s(bio12) + s(bio7), data=sdmdata)
 )
#This "responseInput" name absolutely can't be changed in the current working version.
#This is a bit ugly but I'm not sure what do do I'd like to hide the complexity and it's 
#Generally poor form to assign to the global envt and the runApp needs a consistent input format  
responseInput<-prepareModels(fitLst,inputLayers=layerStk,data=sdmdata,optimal.thresholds=2)

Cols<-c(wes_palette("Darjeeling"),wes_palette("GrandBudapest2"),wes_palette("Cavalcanti"),wes_palette("Moonrise3"))
max_plots<-5
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


responseCurves(f=list(fitLst[[1]]),m=list(modelLst[[2]]),varImp=list(varImpLst[[2]]),addImp=F,vals,dat=responseInput$dat,resp=responseInput$resp)
interactionPlot(fitLst[[1]],modelLst[[1]],vals=NULL,theta=30,phi=25,x="bio1",y="bio5",dat=responseInput$dat,resp=responseInput$resp)
densityPlot(fitLst[[3]])

