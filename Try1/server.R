library(shiny)
library(maptools)


# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
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
      
    obj<-ShapeList[[match(input$Dataset,names(ShapeList))]]
		dat <- readShapePoly(obj)
      var.opts <- names(dat)
    Shape<<-dat #this is global  
		updateSelectInput(session, "Attribute", choices = var.opts)
		})
	
  #===== Select Attribute Value Update =======#	
	observe({
		if (input$Attribute=="Loading...")
			return()
    m<-match(input$Attribute,names(Shape))
      var.opts <- as.character(Shape[[m]])
      
		updateSelectInput(session, "AttributeValue", choices = var.opts)
		})	
    
  output$plotTry<-renderPlot({
    plot(seq(1:10),sqrt(seq(1:10)),cex=seq(1:10),col=seq(1:10),pch=seq(1:10))
  })
   
 
})