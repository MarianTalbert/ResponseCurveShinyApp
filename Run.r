#load the workspace and code for example graphics for now...
#load("J:\\Research\\nccsc\\Private\\Staff\\TalbertM\\Climate\\ParkOutput\\West\\ClimateData\\.RData")
load("H:\\Desktop\\Climate\\ParkOutput\\GLAC\\studyWorkspace")
#ChkLibs(list("maptools","rgdal","raster","ncdf4","fields","maps","RNCEP",
#            "ggplot2","zoo","XML","RCurl","RColorBrewer","chron","ncdf"))
            
setwd("C:\\GoogleDrive\\Interactive\\Rcode\\Shiny\\MyCode")
sourceList<-list("Try1/external/ChkLibs.r","Try1/external/GetParkBoundary.r")
unlist(lapply(sourceList,source))
ChkLibs(list("maptools","rgdal","shiny","leaflet","maptools","rgdal","raster","ncdf4","fields","maps",
            "ggplot2","zoo","XML","RColorBrewer","chron"))

PrismLst<-list()
PrismLst[[1]]<-PrismTmax
PrismLst[[2]]<-PrismTmin
PrismLst[[3]]<-PrismTavg
PrismLst[[4]]<-Prismppt

GDOLst<-list()
GDOLst[[1]]<-GDOTmax
GDOLst[[2]]<-GDOTmin
GDOLst[[3]]<-GDOTavg
GDOLst[[4]]<-GDOPr

MaurerLst<-list()
MaurerLst[[1]]<-MaurerTmax
MaurerLst[[2]]<-MaurerTmin
MaurerLst[[3]]<-MaurerTavg
MaurerLst[[4]]<-Maurerppt

TopoWxLst<-list()
TopoWxLst[[1]]<-TopoTmax
TopoWxLst[[2]]<-TopoTmin
TopoWxLst[[3]]<-TopoTavg
TopoWxLst[[4]]<-NA
runApp("Try1")

runApp("C:\\GoogleDrive\\Interactive\\Rcode\\Shiny\\Examples\\LeonawiczExamples\\shiny-apps-master\\cmip3_cmip5")
runApp("C:\\GoogleDrive\\Interactive\\Rcode\\Shiny\\Examples\\DynamicSelect")
runApp("C:\GoogleDrive\Interactive\Rcode\Shiny\Examples\shiny-examples-master\shiny-examples-master\063-superzip-example")
a<-list.files("C:\\GoogleDrive\\Interactive\\Rcode\\Shiny\\Examples\\LeonawiczExamples\\shiny-apps-master",full.names=TRUE)
