
# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  dat<-NA
  #===== Available Shapefiles Update =======#
  observe({
		if(is.null(input$InputFile))
			return()                                                             
     #add any new shapefiles that have been uploaded to the list
     unzip(input$InputFile$datapath,exdir=TempLoc)
     TempShape<-file.path(TempLoc,gsub(".zip","",input$InputFile$name))
     FileList<-list.files(TempShape,full.names=TRUE) 
     ShapeFile<-FileList[match(".shp",substr(FileList,nchar(FileList)-3,nchar(FileList)))]
     ShapeList[[length(ShapeList)+1]]<-ShapeFile
     names(ShapeList)[length(ShapeList)]<-gsub(".zip","",input$InputFile$name)
   
     ShapeList<<-ShapeList #assign this to global
      updateSelectInput(session, "Dataset", choices = names(ShapeList))
		})
	#======================================	
	 # create the map
   map <- createLeafletMap(session, "map")
   
  #===== Select Attribute Update =======#		
  observe({
		if (input$Dataset=="PleaseSelect")
			return()
			
    #once anything has been selected remove the Please select	
    #if(!is.na(m<-match("PleaseSelect",names(ShapeList)))){ 
    # ShapeList[[m]]<-NULL
     #write to global
    
		#updateSelectInput(session, "Dataset", choices = names(ShapeList))
		#}
	  
    if(!is.null(input$InputFile)){
     #add any new shapefiles that have been uploaded to the list
     ShapeList[[length(ShapeList)+1]]<-input$InputFile$datapath
     names(ShapeList[[length(ShapeList)]])<-input$InputFile$name
    }
  
    ShapePath<-ShapeList[[match(input$Dataset,names(ShapeList))]]
		dat <- readShapePoly(ShapePath)
	
		assign("ShapePath",ShapePath,envir=parent.env(environment()))
      var.opts <- names(dat)
      var.opts<-var.opts[order(var.opts)]
    Shape<<-dat #this is global  
		updateSelectInput(session, "Attribute", choices = var.opts)
		})
	
  #===== Select Attribute Value Update =======#	
	observe({
		if (input$Attribute=="Loading...")
			return()
    m<-match(input$Attribute,names(Shape))
      var.opts <- as.character(Shape[[m]])
      var.opts<-var.opts[order(var.opts)]
		updateSelectInput(session, "AttributeValue", choices = var.opts)
		})	

#====== Once an Attribute value is selected  add it to the map   
  observe({ 

            if(!(input$DisplayShape)) {
              return(NULL)
            } else {
              
            map$clearShapes()
            
            #opts=list(color='#4A9')
             opts=list(color='#FF0080')
              Bounds<-GetParkBoundary(Shape,ShapePath,ParkCode=input$AttributeValue,UNIT_CODE=input$Attribute,Buffer=NA)
              coords<-Bounds@polygons[[1]]@Polygons[[1]]@coords
            map$addPolygon(
              coords[,2],
              coords[,1],
              layerId=c("1"),
              options=opts,
              defaultOptions=opts)
            
           }
        })	

         output$Emissions<-renderPlot({
        if(input$ObsRibbon=="Prism") PastLst<-PrismLst 
        if(input$ObsRibbon=="Maurer") PastLst<-MaurerLst
        if(input$ObsRibbon=="TopoWx") PastLst<-TopoWxLst
        
         if(input$RibbonOrLine=="Ribbon") 
                EmissionSDPlot(GDOLst[[as.numeric(input$Var)]],PastClim=PastLst[[as.numeric(input$Var)]],
                ParkName=ParkName,DisplayOutput=TRUE,OutputGraphics=OutputGraphics,rcp=input$RibbonRCP,
                cexMult=1.2,writeMain=writeMain,Period=5)
         if(input$RibbonOrLine=="Line")
                EmissionLinePlot(GDOLst[[as.numeric(input$Var)]],PastClim=PastLst[[as.numeric(input$Var)]],
                ParkName,DisplayOutput=TRUE,OutputGraphics=OutputGraphics,rcp=input$RibbonRCP,cexMult=1.2,
                writeMain=writeMain,Period=5)
           })
        output$ProjBoxplot<-renderPlot({
        if(input$ObsRibbon=="Prism") PastLst<-PrismLst 
        if(input$ObsRibbon=="Maurer") PastLst<-MaurerLst
        if(input$ObsRibbon=="TopoWx") PastLst<-TopoWxLst
       
             BoxplotRCP(InputDat=GDOLst[[as.numeric(input$Var)]],BaseDat=PastLst[[as.numeric(input$Var)]],Baseline=c(1950,1980),
                BarAvg=20,AllOnePlot=TRUE,Col=NA,DisplayOutput=TRUE,
                OutputGraphics=OutputGraphics,cexMult=1.5,writeMain=TRUE,PlotBase=FALSE,RCP=input$RibbonRCP)
  })
 
#==================================
#==== Historic Trends plots  
  output$HistoricTrends<-renderPlot({ 
       if(input$ObsHist=="Prism") PastLst<-list(PRISM=PrismLst[[as.numeric(input$HistVar)]]) 
       if(input$ObsHist=="Maurer") PastLst<-list(Maurer=MaurerLst[[as.numeric(input$HistVar)]])
       if(input$ObsHist=="TopoWx") PastLst<-list(TopoWx=TopoWxLst[[as.numeric(input$HistVar)]])
       if(input$ObsHist=="CompareHist"){
             PastLst=list(PRISM=PrismLst[[as.numeric(input$HistVar)]],
                    Maurer=MaurerLst[[as.numeric(input$HistVar)]],
                    TopoWx=TopoWxLst[[as.numeric(input$HistVar)]])
                        
                    if(as.numeric(input$HistVar)==4) PastLst<-PastLst[1:2] #no TopoWx for Preci 
                                    }
       TminPlot<-YearlyLinePlot(PastLst,MovAvgPeriod=10,
                   Xlab=(""),
                   MovAvg=input$MovAvg,LM=input$Trend,maCol="blue",
                   DisplayOutput=TRUE,OutputGraphics=OutputGraphics,cexMult=1.6,writeMain=writeMain)
               
  })
 output$MonthlyLine<-renderPlot({ 
       if(input$ObsHist=="Prism") PastLst<-list(PRISM=PrismLst[[as.numeric(input$HistVar)]]) 
       if(input$ObsHist=="Maurer") PastLst<-list(Maurer=MaurerLst[[as.numeric(input$HistVar)]])
       if(input$ObsHist=="TopoWx") PastLst<-list(TopoWx=TopoWxLst[[as.numeric(input$HistVar)]])
       if(input$ObsHist=="CompareHist"){
             PastLst=list(PRISM=PrismLst[[as.numeric(input$HistVar)]],
                    Maurer=MaurerLst[[as.numeric(input$HistVar)]],
                    TopoWx=TopoWxLst[[as.numeric(input$HistVar)]])
                    if(as.numeric(input$HistVar)==3)  browser()       
                     if(as.numeric(input$HistVar)==4) PastLst<-PastLst[1:2] #no TopoWx for Precip     
                                    }
                      
      MonthlyLine(Observational=PastLst,Baseline=input$MonthBase,cexMult=1.6,plotLegend=TRUE)
               
  })
   
  output$AnomalyPlot<-renderPlot({
     if(input$AnomalyHist=="Prism") PastLst<-PrismLst 
       if(input$AnomalyHist=="Maurer") PastLst<-MaurerLst
       if(input$AnomalyHist=="TopoWx") PastLst<-TopoWxLst
       
  AnomalyPlot(PastLst[[as.numeric(input$HistVar)]],Baseline=input$Baseline,ParkName=ParkName,
   DisplayOutput=TRUE,OutputGraphics=OutputGraphics,cexMult=1,writeMain=writeMain)
   })
   
   output$ScatterPlot<-renderPlot({ 
  
      Scatter(GDOTavg,GDOPr,Baseline=input$ScatterBase,rcp=input$ScatterRCP,PlotTime=input$ScatterProj,
    DisplayOutput=TRUE,OutputGraphics=OutputGraphics,
    cexMult=1.3,writeMain=TRUE,addLegend=TRUE)
    })
    
    
      output$ImagePlot<-renderPlot({ 
       if(input$AnomalyHist=="Prism") PastLst<-PrismLst 
       if(input$AnomalyHist=="Maurer") PastLst<-MaurerLst
       if(input$AnomalyHist=="TopoWx") PastLst<-TopoWxLst
       
        ImagePlot(PastLst[[as.numeric(input$HistVar)]],Baseline=input$Baseline,DisplayOutput=TRUE,OutputGraphics=OutputGraphics,cexMult=2.1,writeMain=writeMain)
       })
   
})   
