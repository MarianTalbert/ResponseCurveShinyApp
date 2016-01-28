source("C:\\GoogleDrive\\Interactive\\Rcode\\Shiny\\MyCode\\ResponseCurves\\external\\ggpairs.R")
library(grid)
library(splines)
#Using the biomod data
dat<-myBiomodData@data.env.var
dat$resp<-as.factor(myBiomodData@data.species)

#using the dismo data
dat<-sdmdata[,c(4:7,3)]
dat[,ncol(dat)]<-as.factor(dat[,ncol(dat)])

correlationViewer(sdmdata,layerStk)
correlationViewer(SpDataFrame,myExpl)
ggpairs(SpDataFrame[,c(4:8,3)],alph=.1,pointSize=.5)
