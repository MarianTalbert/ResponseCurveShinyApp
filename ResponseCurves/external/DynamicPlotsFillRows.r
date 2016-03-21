#dat here is the columns of sdmdata minus the lat and lon
#Nplots<-ncol(dat)-1
#n.col<-ncol(dat) #eventually use this but first make sure the brushing even works
Nplots<-10
n.col=5
ui <- shinyUI(fluidPage(
  uiOutput('plots')
))

server <- shinyServer(function(input, output) {
  ranges <- reactiveValues(x = NULL, y = NULL)
  #lapply(1:length(Out$modelLst),function(i){
  #  output[[paste("curves",i,sep="")]] <- renderPlot({        
  plots <- lapply(1:Nplots, function(i){
    renderPlot({
    par(mar=c(1,1,1,1))
    plot(runif(50),main=sprintf('Plot nr #%d',i),xlim = ranges$x, ylim = ranges$y) 
    p <- recordPlot()
    plot.new()
    p})
  })
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })


  output$plots <- renderUI({
    col.width <- round(12/n.col) # Calculate bootstrap column width
    n.row <- ceiling(length(plots)/n.col) # calculate number of rows
    cnter <<- 0 # Counter variable

    # Create row with columns
    rows  <- lapply(1:n.row,function(row.num){
        cols  <- lapply(1:n.col, function(i) {
          cnter    <<- cnter + 1
          plotname <- paste("plot", cnter, sep="")
          column(col.width, plotOutput(plotname, height = 280, width = 250,
                                       dblclick = "plot1_dblclick",
                                       brush = brushOpts(
                                         id = "plot1_brush",
                                         resetOnNew = TRUE
                                       )),style="padding: 1px;")
        }) 
        fluidRow( do.call(tagList, cols),style="padding: 1px;" )
    })

    do.call(tagList, rows)
  })

  for (i in 1:length(plots)) {
    local({
      n <- i # Make local variable
      plotname <- paste("plot", n , sep="")
      output[[plotname]] <- renderPlot({
        plots[[n]]
      })
    })
  }
})

shinyApp(ui=ui,server=server)
