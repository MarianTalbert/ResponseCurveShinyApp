correlationViewer<-function(sdmdata,layerStk,Threshld=.7){              
  #on.exit(return(dat)) #I'll need to reformat this with the selected vars 
  #assuming the data is lon, lat, response, dat
  dat<-sdmdata[,c(4:ncol(sdmdata),3)]
  
  #dat[,ncol(dat)]<-as.factor(dat[,ncol(dat)])
  names(dat)[ncol(dat)]<-"resp"
  
  missing.summary<-1-apply(apply(dat,2,complete.cases),2,sum)/nrow(dat)
  DevScore <- univariateDev(dat)
  
  #calculate total correlations above the threshold
  #and order the data accordingly
  Cors<-cor(dat[,-c(ncol(dat))])
  Cors<-abs(Cors)>Threshld
  TotalCors<-apply(Cors,2,sum)
  dat<-dat[,c(order(TotalCors,decreasing=TRUE),ncol(dat))]
  
  choices<-paste(names(dat)[-c(ncol(dat))]," (","Percent Deviance Explained ",DevScore$devExp,"%)",sep="")
#maybe put in app eventually                   
app <- shinyApp(
    ui = navbarPage("Input Data Explorer",
      tabPanel("Correlation Filtering",
        sidebarLayout(
            sidebarPanel(
              radioButtons(inputId="sortBy","Sort by:",c("Total high correlations with other variables being considered"="TotCors",
                                                        "Percent deviance explained in a univariate GAM model"="Dev")),
              uiOutput("varChoices"),
              checkboxInput("showRespGam","Show GAM fit each predictor with the Response",value=FALSE),
              numericInput("numPlts","Number of variables to display" , 5),
              sliderInput("pointSize","Scatterplot point size",min=.05,max=6,value=1),
              sliderInput("alpha","Scatterplot point transparency",min=.05,max=1,value=.7),
              actionButton("highlight", label = "Highlight points in brushed area"),
              actionButton("resethighlight", label = "remove highlight selection")
          ),
          mainPanel(
            uiOutput('plots')
          )
        )
      ),
     tabPanel("Variable Plot",
        sidebarLayout(
          sidebarPanel(
            uiOutput("oneVarChoice"),
            checkboxGroupInput("showTrain","Show Calibrarion Data",
                               c("add presence points"="showPres",
                                 "add absence/background points"="showAbs")),
            sliderInput("mpPtSz", "Map pointsize", 
                        min=0, max=5, value=.7,step=.1)
        ),
        mainPanel(
          fluidRow(
            column(10,
          plotOutput("VarMap",height=600,width=600))
          ),
          fluidRow(
            column(5,
                   plotOutput("Gam",height=400,width=400)),
          column(5,plotOutput("Hist",height=400,width=400))
          )
        )
     ))  
    ),
    server = function(input, output) {
      XYs <- reactiveValues(
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
            if(colNum==0) colNum<-n.col+ShowResp
            #use the zero column for the relationship bw resp and pred if it is to be used
            if(ShowResp) colNum<-colNum-1 
            
            ggpairs(dat,alph=.5,pointSize=1,DevScore=2,showResp=ShowResp,brushRegion=XYs$brushRegion,
                    rowNum=rowNum,colNum=colNum)
            
          })
        })
      }
      
      observeEvent(input$resethighlight,{
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
        Yvar<-ceiling(as.numeric(plotNum)/(n.col+ShowResp))
        Xvar<-as.numeric(plotNum)%%(n.col+ShowResp)-ShowResp
        
        if (!is.null(brush)) {
          #three cases for the response look at the X and the correct response
          #for the hist the Yvar is wrong because it's a count
          
          #X and y are actually different for the first column
          if(Xvar==0) XYs$brushRegion<-dat[,Yvar]<=brush$xmax & dat[,Yvar]>=brush$xmin &
              dat[,ncol(dat)]<=brush$ymax & dat[,ncol(dat)]>=brush$ymin
          if(Xvar==Yvar) XYs$brushRegion<-dat[,Xvar]<=brush$xmax & dat[,Xvar]>=brush$xmin
          if(Xvar!=0 & Xvar!=Yvar) XYs$brushRegion<-dat[,Xvar]<=brush$xmax & dat[,Xvar]>=brush$xmin &
              dat[,Yvar]<=brush$ymax & dat[,Yvar]>=brush$ymin
          
        } else {
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
      values<-reactiveValues(orderVar=TotalCors,
                             dat=dat,
                             devExp=DevScore$devExp,
                             TotCors=TotalCors)
      
      observeEvent(input$sortBy,{
        
        if(input$sortBy=="TotCors") o<-order(TotalCors,decreasing=TRUE)
        else o<-order(DevScore$devExp,decreasing=TRUE)
          values$dat<-dat[,c(o,ncol(dat))]
          values$devExp<-DevScore$devExp[o]
          values$TotCors<-TotalCors[o]
      }) 
      
      output$varChoices <- renderUI({
        choices<-paste(names(values$dat)[-c(ncol(values$dat))],
                               " (","Percent Deviance Explained ",values$devExp,"%)",sep="")
        checkboxGroupInput('chkGrp', 'Variables to include', choices=choices,selected=choices)
 
      })
      
      output$oneVarChoice <- renderUI({
        choices<-paste(names(values$dat)[-c(ncol(values$dat))],
                       " (","Percent Deviance Explained ",values$devExp,"%)",sep="")
       radioButtons('InptVar', 'Variable', choices=choices)
      })
  
     
      output$VarMap<-renderPlot({
        if(!is.null(input$InptVar)){
          InputVar<-strsplit(input$InptVar," ")[[1]][1]
          par(oma=c(0,0,0,0),mar=c(2,2,2,2))
          plot(layerStk, match(InputVar,names(layerStk)))
          #probably use a choice of maps here
          plot(wrld_simpl, add=TRUE,cex.main=3)
          if(!is.null(input$showTrain)){
            
            #input$varptSize
            if("showPres"%in%input$showTrain) points(sdmdata[sdmdata[,3]==1,1],
                                                     sdmdata[sdmdata[,3]==1,2], 
                                                     col="deeppink",bg="red",pch=21,cex=input$mpPtSz)
            if("showAbs"%in%input$showTrain) points(sdmdata[sdmdata[,3]==0,1],
                                                    sdmdata[sdmdata[,3]==0,2],
                                                    col="dodgerblue", bg="blue",pch=21,cex=input$mpPtSz)
          } 
       }
      })
      
                              
    output$Gam<-renderPlot({
      if(!is.null(input$InptVar)){
        InputVar<-strsplit(input$InptVar," ")[[1]][1]
        varNum<-match(InputVar,names(values$dat))
        respPlt<-ggplot(values$dat, aes_q(x = as.name(InputVar), 
                                   y =as.name("resp"),
                                   colour=as.name(InputVar))) + 
          geom_point(alpha=.4) +
          stat_smooth(method="glm", method.args=list(family="binomial"), formula = y ~ ns(x, 2))+
          scale_color_gradient(low="blue",high="red")+theme(legend.position="none")+
          theme(panel.grid.minor=element_blank(),
               panel.grid.major=element_blank(),plot.margin=unit(c(3,4,3,2),"mm"))+
          xlab("")+ylab(paste(ifelse(DevScore$GamRan[varNum],"GAM","GLM"), "% Dev Expl",DevScore$devExp[varNum]))+
          scale_y_continuous(breaks=NULL)+ggtitle("Response")
        
        respPlt
      }
    })
    output$Hist<-renderPlot({
      if(!is.null(input$InptVar)){
        InputVar<-strsplit(input$InptVar," ")[[1]][1]
        ggplot(values$dat, aes_q(x=as.name(InputVar),
                          fill=as.factor(values$dat$resp)))+
          geom_histogram()+theme(plot.margin=unit(c(0,0,0,0),"mm"))+
          scale_fill_manual(values=c("blue","red"),name="Response")+
          scale_y_discrete(breaks=NULL)+scale_x_continuous(breaks=NULL)
      }
    })
      
    }
  )
           
runApp(app)   

}