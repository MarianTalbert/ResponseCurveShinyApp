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
gam::gam(pb~bio1,dat)



  ggplot(sdmdata, aes_q(x=as.name("bio1"),
                    fill=as.factor(sdmdata$pb)))+geom_histogram()+theme(plot.margin=unit(c(0,0,0,0),"mm"))+
    scale_fill_manual(values=c("blue","red"),name="Response")
           
  ggplot(upc, aes(x=reorder(Abb, Change), y=Change, fill=Region)) +
    geom_bar(stat="identity", colour="black") +
    scale_fill_manual(values=c("#669933", "#FFCC66"))