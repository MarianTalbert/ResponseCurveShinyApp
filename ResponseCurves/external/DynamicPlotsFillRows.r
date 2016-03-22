Nplots<-10
n.col<-5
ui <- shinyUI(fluidPage(
  actionButton("zoom", label = "Zoom to brushed area"),
  actionButton("resetzoom",label = "return to full extent"),
  actionButton("highlight", label = "Highlight points in brushed area"),
  actionButton("resethighlight", label = "remove hight selection"),
  uiOutput('plots')
))

server <- shinyServer(function(input, output) {

  XYs <- reactiveValues(
    xlim = NULL,
    ylim = NULL,
    xhighlight =NULL,
    yhighlight = NULL
  )
  for (i in 1:Nplots) {
    local({
      n <- i # Make local variable
      plotname <- paste("plot", n , sep="")
      output[[plotname]] <- renderPlot({
        par(mar=c(1,1,1,1))
        plot(runif(50),main=sprintf('Plot nr #%d',i),xlim=XYs$xlim,ylim=XYs$ylim) 
      })
    })
  }
#   plots <- lapply(1:Nplots, function(i){
#     output[[paste("plot",i,sep="")]] <-renderPlot({
#       browser()
#     par(mar=c(1,1,1,1))
#     plot(runif(50),main=sprintf('Plot nr #%d',i),xlim=XYs$xlim,ylim=XYs$ylim) 
#     p <- recordPlot()
#     plot.new()
#     p})
#   })
  
  
  observeEvent(input$zoom,{
    brush <- input$plotbrush
    if (!is.null(brush)) {
      XYs$xlim <- c(brush$xmin, brush$xmax)
      XYs$ylim <- c(brush$ymin, brush$ymax)
      
    } else {
      XYs$xlim <- NULL
      XYs$ylim <- NULL
    }
  })
  
  observeEvent(input$highlight,{
    brush <- input$plotbrush
    if (!is.null(brush)) {
      XYs$xhighlight <- c(brush$xmin, brush$xmax)
      XYs$yhighlight <- c(brush$ymin, brush$ymax)
      
    } else {
      XYs$xhighlight <- NULL
      XYs$yhighlight <- NULL
    }
  })
  
  output$plots <- renderUI({
    col.width <- round(12/n.col) # Calculate bootstrap column width
    n.row <- ceiling(Nplots/n.col) # calculate number of rows
    cnter <<- 0 # Counter variable
    
    # Create row with columns
    rows  <- lapply(1:n.row,function(row.num){
        cols  <- lapply(1:n.col, function(i) {
          cnter    <<- cnter + 1
          plotname <- paste("plot", cnter, sep="")
          column(col.width, plotOutput(plotname,height="300px",
                                       brush = brushOpts(
                                         id = "plotbrush",
                                         resetOnNew = TRUE
                                       )),style="padding: 1px;")
        }) 
        fluidRow( do.call(tagList, cols),style="padding: 1px;" )
    })

    do.call(tagList, rows)
  })

 
})

shinyApp(ui=ui,server=server)
