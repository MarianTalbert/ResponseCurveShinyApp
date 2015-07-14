setwd("C:\\GoogleDrive\\Python\\DevWorkspace\\userpackages\\sahm\\pySAHM\\Resources\\R_Modules")
ScriptPath="C:\\GoogleDrive\\Python\\DevWorkspace\\userpackages\\sahm\\pySAHM\\Resources\\R_Modules"


source("LoadRequiredCode.r")
source("MARS.helper.fcts.r")
source("GLM.helper.fcts.r")
source("BRT.helper.fcts.r")
source("RF.helper.fcts.r")
source("MAXENT.helper.fcts.r")
setwd("C:\\GoogleDrive\\Interactive\\Rcode\\Shiny\\MyCode")
sourceList<-list("ResponseCurves\\external\\ChkLibs.r","ResponseCurves\\external\\Colors.r","ResponseCurves\\external\\response.curves.r")
unlist(lapply(sourceList,source))
ChkLibs(list("gbm","randomForest","maptools","rgdal","shiny","leaflet","maptools","rgdal","raster","ncdf4","fields","maps",
            "ggplot2","zoo","XML","RColorBrewer","chron","wesanderson"))


output.dir="C:\\temp\\SAHM_workspace"
rc="responseBinary"
wsLst<-list()
wsLst[[1]]<-"C:\\temp\\SAHM_workspace\\ForResponseCurveTool\\brewersSparrow\\brt_1\\modelWorkspace"
wsLst[[2]]<-"C:\\temp\\SAHM_workspace\\ForResponseCurveTool\\brewersSparrow\\glm_1\\modelWorkspace"
wsLst[[3]]<-"C:\\temp\\SAHM_workspace\\ForResponseCurveTool\\brewersSparrow\\mars_1\\modelWorkspace"
wsLst[[4]]<-"C:\\temp\\SAHM_workspace\\ForResponseCurveTool\\brewersSparrow\\rf_1\\modelWorkspace"

fitLst<-list()
modelLst<-list()
mapLst<-vector()
rastLst<-vector()
for(w in 1:length(wsLst)){
 load(wsLst[[w]])
  modelLst[[w]]<-out$input$script.name
  rastLst<-out$dat$tif.ind
  if(w>1 & any(rastLst!=out$dat$tif.ind)) stop("Rasters Don't match for all workspaces")
  fitLst[[w]]<-out
  rm(out)
  mapLst[[w]]<-file.path(dirname(wsLst[[w]]),paste(modelLst[[w]],"prob_map.tif",sep="_"))
}
mapStk<<-stack(mapLst)
stk<-stack(rastLst)
vals<-extract(stk,cbind(x,y))


#reading in the map and converting it to a ggplot format
map<-raster("C:\\temp\\SAHM_workspace\\rf_7_prob_map.tif")
map<-aggregate(map,by=10)
#convert the raster to points for plotting
map.p <- rasterToPoints(map)
#Make the points a dataframe for ggplot
ras <- data.frame(map.p)
#Make appropriate column headings
colnames(ras) <- c("Longitude", "Latitude", "MAP")
Cols<<-c(wes_palette("Darjeeling"),wes_palette("Moonrise3"))
max_plots<-5
nModels<<-4
#=========================================
#    This is where the ma
runApp("ResponseCurves")


#=========================================
# scratch pad 
vals<-rbind(c(.2,.1,50,-10,.2,.06,.5,2),
c(.9,-.6,50,-10,.2,.06,.5,2),
c(.2,.1,50,-10,.2,0,.17,2),
c(.2,.1,50,-10,.2,.06,.5,0))
response.curves(fitLst,modelLst,vals)
response.curvesOneModel(fitLst[[2]],modelLst[[2]],vals) 