library(shiny)
library(maptools)
# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {

  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  
  observe({
		if (is.null(input$Dataset))
			return()
		obj<-switch(input$Dataset,
           NpsShapes = "H:\\Desktop\\Climate\\InputLayers\\NPS_boundaries\\nps_boundary.shp", 
         StateBounds = "H:\\Desktop\\Climate\\InputLayers\\StateBounds\\statep010.shp")
		dat <- readShapePoly(obj)
      var.opts <- names(dat)
    #output$Shape<-dat  
		updateSelectInput(session, "Attribute", choices = var.opts)
		updateSelectInput(session, "group", choices = var.opts)
		})
		
	#observe({
	#	if (is.null(input$Attribute))
	#		return()
	#	m<-match(names(output$Shape),output$Attribute)
#		browser()
#    names(output$Shape)
#		dat <- readShapePoly(obj)
#      var.opts <- names(dat)
#		updateSelectInput(session, "Attribute", choices = var.opts)
#		updateSelectInput(session, "group", choices = var.opts)
#		})	
    
  output$plotTry<-renderPlot({
    plot(seq(1:10),sqrt(seq(1:10)),cex=seq(1:10),col=seq(1:10),pch=seq(1:10))
  })
   
 
})