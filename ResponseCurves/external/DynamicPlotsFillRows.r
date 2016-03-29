Nplots<-10
n.col<-5
#now trying to use actual data and ggplot for pairs
dat<-sdmdata[,c(4:ncol(sdmdata),3)]
ShowResp<-TRUE
Nplots<-(ncol(dat)-1)^2+ifelse(ShowResp,(ncol(dat)-1),0)
n.col<-ncol(dat)-1
#colNum<-num %% (n.col+1)
#rowNum<-ceiling(num/n.col)

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
    yhighlight = NULL,
    brushRegion = NULL
  )
  for (i in 1:Nplots) {
    local({
      num <- i # Make local variable
      plotname <- paste("plot", num , sep="")
      output[[plotname]] <- renderPlot({
        colNum<-num %% (n.col+ShowResp)
        rowNum<-ceiling(num/(n.col+ShowResp))
        #modular arithmitic doesn't quite map how I need
        if(colNum==0) colNum<-n.col+1
        #use the zero column for the relationship bw resp and pred if it is to be used
        if(ShowResp) colNum<-colNum-1 
        if(!is.null(XYs$xlim) | !is.null(XYs$xhighlight)) browser()
        ggpairs(dat,alph=.5,pointSize=1,DevScore=2,showResp=ShowResp,brushRegion=XYs$brushRegion,
                rowNum=rowNum,colNum=colNum)
        #plot(runif(50),main=sprintf('Plot nr #%d',i),xlim=XYs$xlim,ylim=XYs$ylim) 
      })
    })
  }

  
  observeEvent(input$zoom,{
    
    #I have to figure out which of my plotbrushes was used and
    #store the number to calculate indicies associated with limits
    Name<-names(input)[grep("plotbrush",names(input))]
    brush<-input[[Name]]
    plotNum<-gsub("plotbrush","",Name)
    
    if (!is.null(brush)) {
      XYs$xlim <- c(brush$xmin, brush$xmax)
      XYs$ylim <- c(brush$ymin, brush$ymax)
      XYs$plotNum <-plotNum
      
    } else {
      XYs$xlim <- NULL
      XYs$ylim <- NULL
      XYs$plotNum <-NULL
    }
  })
  
  observeEvent(input$highlight,{
  
    Name<-names(input)[grep("plotbrush",names(input))]
    brush<-input[[Name]]
    plotNum<-gsub("plotbrush","",Name)
    browser()
    #I need to make sure this works 
    Yvar<-ceiling(as.numeric(plotNum)/(n.col+ShowResp))
    Xvar<-as.numeric(plotNum)%%(n.col-ShowResp)+1
   
    if (!is.null(brush)) {
      
      XYs$xhighlight <- c(brush$xmin, brush$xmax)
      XYs$yhighlight <- c(brush$ymin, brush$ymax)
      XYs$plotNum <-plotNum
      XYs$brushRegion<-dat[,Xvar]<=brush$xmax & dat[,Xvar]>=brush$xmin &
                       dat[,Yvar]<=brush$ymax & dat[,Yvar]>=brush$ymin
      
    } else {
      XYs$xhighlight <- NULL
      XYs$yhighlight <- NULL
      XYs$plotNum <-NULL
      XYs$brushRegion <-NULL
    }
  })
  
  output$plots <- renderUI({
    col.width <- round(12/(n.col+ShowResp)) # Calculate bootstrap column width
    n.row <- ceiling(Nplots/(n.col+ShowResp)) # calculate number of rows
    cnter <<- 0 # Counter variable
   
    # Create row with columns
    rows  <- lapply(1:n.row,function(row.num){
        cols  <- lapply(1:(n.col+ShowResp), function(i) {
          cnter    <<- cnter + 1
          brushName<-paste("plotbrush",cnter,sep="")
          plotname <- paste("plot", cnter, sep="")
          column(col.width, plotOutput(plotname,height="300px",
                                       brush = brushOpts(
                                         id = brushName,
                                         resetOnNew = TRUE
                                       )),style="padding: 1px;")
        }) 
        fluidRow( do.call(tagList, cols),style="padding: 1px;" )
    })

    do.call(tagList, rows)
  })

 
})

shinyApp(ui=ui,server=server)
