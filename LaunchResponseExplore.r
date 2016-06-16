setwd("C:\\GoogleDrive\\Interactive\\Rcode\\Shiny\\MyCode")
ShinyCode<-file.path("ResponseCurves\\External")
sourceList<-list.files(ShinyCode,full.names=TRUE)
unlist(lapply(as.list(sourceList),source))

ChkLibs(list("rgeos","maptools","randomForest","mgcv","dismo","shiny","earth","PresenceAbsence",
             "wesanderson","ggplot2","raster","grid","gridExtra","splines","RColorBrewer","viridis"))
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
 
 head(sdmdata)
 
correlationViewer(sdmdata,layerStk) 
correlationViewer(data=sdmdata)

#put the desired models in a list

fitLst<-list(
 GLM_Model = glm(pb ~ bio1 + bio5 + bio12+ bio7, data=sdmdata,family=binomial),
 MARS_Model = earth(pb~ bio1 + bio5 + bio12 + bio7, data=sdmdata,glm=list(family=binomial)),
 RF_Model=randomForest(pb~ bio1 + bio5 + bio12 + bio7,data=sdmdata, norm.votes = TRUE, strata = factor(c(0,1)), 
                       nodesize = 30),
 GAM_Model=gam(pb ~ 1+s(bio1,k=-1) + s(bio5,k=-1) + s(bio12,k=-1) + s(bio7,k=2), data=sdmdata,family=binomial)
 )
c('GLM','GBM','GAM','CTA','ANN','SRE','FDA','MARS','RF','MAXENT') 
nn<-nnet(as.factor(pb)~ bio1 + bio5 + bio12+ bio7, data=sdmdata,size=25)
predict(nn) #this works but the "response" arg in my predict will break it
#look at caret package tomorrow for streamlining this sort of thing
#=============================================================
#    This is where the magic happens
#
#just for now returning the output so I can beautify my plots
exploreCurves(fitLst,inputLayers=layerStk,data=sdmdata,threshold=2,boundary=wrld_simpl)
#
#===============================================================
#Debugging helper functions
vals<-apply(sdmdata[,c(4:ncol(sdmdata))],2,FUN=sample,size=4)
varI<-as.data.frame(matrix(runif(16,0,1),nrow=4,ncol=4))
varIncl<-varI>.25

responseCurves(f=fitLst,model=modelLst,vals=vals,
               varImp=as.list(varI),varIncluded=as.list(varIncl),addImp=F,pIdx=1,
               dat=sdmdata[,c(4:ncol(sdmdata))],resp=sdmdata[,3],Cols=Cols,Ensemble=FALSE)

interactionPlot(fitLst[[1]],model=modelLst[[1]],vals=NULL,theta=30,phi=25,x=names(sdmdata)[4],y=names(sdmdata)[5],
                dat=sdmdata[,c(4:ncol(sdmdata))],resp=sdmdata[,3])

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
Cols<-c(wes_palette("Darjeeling"),wes_palette("GrandBudapest2"),wes_palette("Cavalcanti"),wes_palette("Moonrise3"))
vals<-apply(sdmdata[,c(4:ncol(sdmdata))],2,FUN=sample,size=4)
modelLst<-names(fitLst)
varImpLst<-NA



