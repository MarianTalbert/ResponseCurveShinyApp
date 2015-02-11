#load the workspace and code for example graphics for now...
load("C:\\Users\\mtalbert\\Downloads\\Example.RData")
ChkLibs(list("maptools","rgdal","raster","ncdf4","fields","maps","RNCEP",
            "ggplot2","zoo","XML","RCurl","RColorBrewer","chron","ncdf"))
            
setwd("C:\\GoogleDrive\\Interactive\\Rcode\\Shiny\\MyCode")
sourceList<-list("Try1/external/ChkLibs.r","Try1/external/GetParkBoundary.r")
unlist(lapply(sourceList,source))
ChkLibs(list("shiny","leaflet","maptools","rgdal","raster","ncdf4","fields","maps",
            "ggplot2","zoo","XML","RColorBrewer","chron"))

runApp("Try1")

runApp("C:\\GoogleDrive\\Interactive\\Rcode\\Shiny\\Examples\\LeonawiczExamples\\shiny-apps-master\\cmip3_cmip5")
runApp("C:\\GoogleDrive\\Interactive\\Rcode\\Shiny\\Examples\\DynamicSelect")
runApp("C:\GoogleDrive\Interactive\Rcode\Shiny\Examples\shiny-examples-master\shiny-examples-master\063-superzip-example")
a<-list.files("C:\\GoogleDrive\\Interactive\\Rcode\\Shiny\\Examples\\LeonawiczExamples\\shiny-apps-master",full.names=TRUE)
