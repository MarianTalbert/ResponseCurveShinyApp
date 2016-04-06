correlationViewer<-function(sdmdata,layerStk,Threshld=.7){              
  #on.exit(return(dat)) #I'll need to reformat this with the selected vars 
  #assuming the data is lon, lat, response, dat
  startNumPlts<-5
  dat<-sdmdata[,c(4:ncol(sdmdata),3)]
  
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
              checkboxInput("showRespGam","Show GAM fit each predictor with the Response",value=TRUE),
              radioButtons(inputId="sortBy","Sort by:",
                           c("Total high correlations with other variables being considered"="TotCors",
                              "Percent deviance explained in a univariate GAM model"="Dev")),
              uiOutput("varChoices"),
              numericInput("numPlts","Number of variables to display" , startNumPlts),
              sliderInput("pointSize","Scatterplot point size",min=.05,max=6,value=1),
              sliderInput("alpha","Scatterplot point transparency",min=.05,max=1,value=.7),
              actionButton("highlight", label = "Highlight points in brushed area"),
              actionButton("resethighlight", label = "remove highlight selection"),width=2
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
                        min=0, max=5, value=.7,step=.1),
            actionButton("resetExtent",label = "Reset spatial extent" ),width=3
        ),
        mainPanel(
          fluidRow(
            column(10,
          plotOutput("VarMap",height=600,width=600,brush = brushOpts(
            id = "mapbrush",
            resetOnNew = TRUE
          )))
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
        brushRegion = rep(FALSE,times=nrow(dat)),
          xlim = c(xmin(layerStk),xmax(layerStk)),
          ylim = c(ymin(layerStk),ymax(layerStk))
      )
      
      values<-reactiveValues(
        orderVar=TotalCors,
        dat=dat,
        devExp=DevScore$devExp,
        TotCors=TotalCors,
        VarsToUse=names(dat)[1:(ncol(dat)-1)],
        Nplots=min(startNumPlts,(ncol(dat)-1))^2+(min(startNumPlts,ncol(dat)-1)),
        n.col=min(ncol(dat)-1,startNumPlts)
      )
      
      observeEvent(input$numPlts,{
           values$Nplots<-min(input$numPlts,(ncol(values$dat)-1))^2+
                 ifelse(input$showRespGam,min(input$numPlts,(ncol(values$dat)-1)),0)
        })
     
      #setting single predictor brush extent
      observeEvent(input$mapbrush,{
        brush <- input$mapbrush
        if (!is.null(brush)) {
          XYs$xlim <- c(brush$xmin, brush$xmax)
          XYs$ylim <- c(brush$ymin, brush$ymax)
          
        } else {
          XYs$xlim <- c(xmin(layerStk),xmax(layerStk))
          XYs$ylim <- c(ymin(layerStk),ymax(layerStk))
        }
      })
      
      observeEvent(input$resetExtent,{
        XYs$xlim <- c(xmin(layerStk),xmax(layerStk))
        XYs$ylim <- c(ymin(layerStk),ymax(layerStk))
      })
     
      observeEvent(input$chkGrp,{
         values$varsToUse<-names(values$dat)[1:(ncol(values$dat)-1)]  
          if(!is.null(input$chkGrp)){
               spltDat<-strsplit(input$chkGrp," ")
               values$varsToUse<-unlist(lapply(spltDat,"[",1))
          }
          
      })
      
      #both of these are needed where I can't use reactive
      Nplots<-isolate(values$Nplots)
      
      n.col<-reactive({min(ncol(values$dat)-1,input$numPlts)})
      #===================================================
      #Interactive pairs plot
     
      output$plots <- renderUI({
        Nplots<-values$Nplots
        for (i in 1:as.numeric(Nplots)) {
          
          local({
            num <- i # Make local variable
            plotname <- paste("plot", num , sep="")
            Nplots<-as.numeric(Nplots)
            output[[plotname]] <- renderPlot({
              
              colNum<-num %% (n.col()+input$showRespGam)
              rowNum<-ceiling(num/(n.col()+input$showRespGam))
              #modular arithmitic doesn't quite map how I need
              if(colNum==0) colNum<-n.col()+input$showRespGam
              #use the zero column for the relationship bw resp and pred if it is to be used
              
              vtu<-c(names(values$dat)[names(values$dat)%in%values$varsToUse],"resp")
              values$dat<-values$dat[,names(values$dat)%in%vtu]
              
              if(input$showRespGam) colNum<-colNum-1 
              #if(input$numPlts!=5 & num==1) browser()
              ggpairs(values$dat[,c(1:min(input$numPlts,(ncol(values$dat)-1)),ncol(values$dat))],
                      alph=input$alpha,pointSize=input$pointSize,DevScore=2,
                      showResp=input$showRespGam,brushRegion=XYs$brushRegion,
                      rowNum=rowNum,colNum=colNum)
              
            })
          })
        }
        col.width <- round(10/(n.col()+input$showRespGam)) # Calculate bootstrap column width
        n.row <- ceiling(values$Nplots/(n.col()+input$showRespGam)) # calculate number of rows
        cnter <<- 0 # Counter variable
        rowStyle<-paste("padding: 1px;height: ",100*col.width,"px;",sep="")
        # Create row with columns
        rows  <- lapply(1:n.row,function(row.num){
          cols  <- lapply(1:(n.col()+input$showRespGam), function(i) {
            cnter    <<- cnter + 1
            brushName<-paste("plotbrush",cnter,sep="")
            plotname <- paste("plot", cnter, sep="")
            column(col.width, plotOutput(plotname,
                                         brush = brushOpts(
                                           id = brushName,
                                           resetOnNew = TRUE
                                         ),height="200px"),
                   style="height:200px;padding: 1px")
          }) 
          fluidRow( do.call(tagList, cols),style="padding: 1px")
        })
        
        do.call(tagList, rows)
      })
      
      #===========================================
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
        Yvar<-ceiling(as.numeric(plotNum)/(n.col()+input$showRespGam))
        Xvar<-as.numeric(plotNum)%%(n.col()+input$showRespGam)-input$showRespGam
        
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
      
     
     
      
     observeEvent(input$sortBy,{
        
        if(input$sortBy=="TotCors") o<-order(TotalCors,decreasing=TRUE)
        else o<-order(DevScore$devExp,decreasing=TRUE)
          values$dat<-dat[,c(o,ncol(dat))]
          values$devExp<-DevScore$devExp[o]
          values$TotCors<-TotalCors[o]
      }) 
      
     output$varChoices <- renderUI({
        choices<-paste(names(dat)[-c(ncol(dat))],
                               " (","Percent Deviance Explained ",values$devExp,"%)",sep="")
        checkboxGroupInput('chkGrp', 'Variables to include', choices=choices,selected=choices)
 
      })
      
     output$oneVarChoice <- renderUI({
        choices<-paste(names(dat)[-c(ncol(dat))],
                       " (","Percent Deviance Explained ",values$devExp,"%)",sep="")
       radioButtons('InptVar', 'Variable', choices=choices)
      })
  
     output$VarMap<-renderPlot({
        if(!is.null(input$InptVar)){
          InputVar<-strsplit(input$InptVar," ")[[1]][1]
          par(oma=c(0,0,0,0),mar=c(2,2,2,2))
          plot(layerStk, match(InputVar,names(layerStk)),cex.main=1.7,xlim=XYs$xlim,ylim=XYs$ylim)
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