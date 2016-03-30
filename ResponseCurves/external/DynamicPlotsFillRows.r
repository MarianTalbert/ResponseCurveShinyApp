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
  actionButton("highlight", label = "Highlight points in brushed area"),
  actionButton("resethighlight", label = "remove highlight selection"),
  uiOutput('plots')
))

server <- shinyServer(function(input, output) {

  XYs <- reactiveValues(
    xlim = NULL,
    ylim = NULL,
    xhighlight =NULL,
    yhighlight = NULL,
    brushRegion = rep(FALSE,times=nrow(dat))
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
        
        ggpairs(dat,alph=.5,pointSize=1,DevScore=2,showResp=ShowResp,brushRegion=XYs$brushRegion,
                rowNum=rowNum,colNum=colNum)
        #plot(runif(50),main=sprintf('Plot nr #%d',i),xlim=XYs$xlim,ylim=XYs$ylim) 
      })
    })
  }

  observeEvent(input$resethighlight,{
    XYs$xlim = NULL
    XYs$ylim = NULL
    XYs$xhighlight =NULL
    XYs$yhighlight = NULL
    XYs$brushRegion = rep(FALSE,times=nrow(dat))
  })
  observeEvent(input$highlight,{
  
    Name<-names(input)[grep("plotbrush",names(input))]
    
    #shiny gets angry if I check for null in an lapply so do a loop over names
    #to figure out which plotbrush is non-null
    for(i in 1:length(Name)) if(!is.null(input[[Name[i]]])) indx=i
    Name<-Name[indx]
    brush<-input[[Name]]
    plotNum<-gsub("plotbrush","",Name)
    
    #I need to make sure this works 
    Xvar<-floor(as.numeric(plotNum)/(n.col+ShowResp))
    Yvar<-as.numeric(plotNum)%%(n.col+ShowResp)
    
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
      XYs$brushRegion <-rep(FALSE,times=nrow(dat))
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
