setwd("C:\\GoogleDrive\\Interactive\\Rcode\\Shiny\\MyCode")
sourceList<-list("Try1/external/ChkLibs.r","Try1/external/GetParkBoundary.r")
unlist(lapply(sourceList,source))


runApp("Try1")

runApp("C:\\GoogleDrive\\Interactive\\Rcode\\Shiny\\Examples\\LeonawiczExamples\\shiny-apps-master\\cmip3_cmip5")
runApp("C:\\GoogleDrive\\Interactive\\Rcode\\Shiny\\Examples\\DynamicSelect")
runApp("C:\GoogleDrive\Interactive\Rcode\Shiny\Examples\shiny-examples-master\shiny-examples-master\063-superzip-example")
a<-list.files("C:\\GoogleDrive\\Interactive\\Rcode\\Shiny\\Examples\\LeonawiczExamples\\shiny-apps-master",full.names=TRUE)
