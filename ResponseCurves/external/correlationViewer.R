correlationViewer<-function(sdmdata,layerStk,boundary=NA,Threshld=.7){              
  #on.exit(return(dat)) #I'll need to reformat this with the selected vars 
  #assuming the data is lon, lat, response, dat
  startNumPlts<-5
  dat<-sdmdata[,c(4:ncol(sdmdata),3)]
  
  names(dat)[ncol(dat)]<-"resp"
  
  missing.summary<-1-apply(apply(dat,2,complete.cases),2,sum)/nrow(dat)
  DevScore <- univariateDev(dat)

  names(DevScore$devExp)<-names(dat)[1:(ncol(dat)-1)]
  #calculate total correlations above the threshold
  #and order the data accordingly
  Cors<-cor(dat[,-c(ncol(dat))])
  Cors<-abs(Cors)>Threshld
  TotalCors<-apply(Cors,2,sum)
 
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
              numericInput(inputId="numPlts",label="Number of variables to display" , 
                           value=startNumPlts,max=5),
              sliderInput("pointSize","Scatterplot point size",min=.05,max=6,value=1),
              sliderInput("alpha","Scatterplot point transparency",min=.05,max=1,value=.7),
              actionButton("highlight", label = "Highlight points in brushed area"),
              actionButton("resethighlight", label = "remove highlight selection"),
              uiOutput("varChoices"),
              helpText("click the done button to return your choices"),
              actionButton(inputId="done", "Return Selection"),
              width=2
           ),
           
              
          mainPanel(
            uiOutput('plots'),
          width=10)
        )
      ),
     tabPanel("Variable Plot",
        sidebarLayout(
          sidebarPanel(
            checkboxGroupInput("showTrain","Show Calibrarion Data",
                               c("add presence points"="showPres",
                                 "add absence/background points"="showAbs")),
            sliderInput("mpPtSz", "Point size for map points", 
                        min=0, max=5, value=.7,step=.1),
            sliderInput("mpPtTr", "Point transparency", 
                        min=0, max=1, value=.7,step=.1),
            sliderInput("mpTr", "Map transparency", 
                        min=0, max=1, value=1,step=.1),
            actionButton("resetExtent",label = "Reset spatial extent" ),
            uiOutput("oneVarChoice"),
            width=2
        ),
        mainPanel(
          fluidRow(
            column(6,
                plotOutput("VarMap",brush = brushOpts(
                id = "mapbrush",
                resetOnNew = TRUE
            ))),
            column(6,plotOutput("Hist")),
            column(4,
                   plotOutput("Gam",height=300,width=300))
          
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
        n.col=min(startNumPlts,(ncol(dat)-1))
        )
      
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
      observe({
        
        if(input$done > 0){
          VarsToUse <- values$varsToUse
          indToReturn <- c(1,2,3,match(VarsToUse,names(sdmdata)))
          stopApp(returnValue=indToReturn)
          
        }
      })
      
      observeEvent(input$resetExtent,{
        XYs$xlim <- c(xmin(layerStk),xmax(layerStk))
        XYs$ylim <- c(ymin(layerStk),ymax(layerStk))
      })
     
     observe({
        values$varsToUse<-names(values$dat)[1:(ncol(values$dat)-1)]  
         if(!is.null(input$chkGrp)){
              spltDat<-strsplit(input$chkGrp," ")
              values$varsToUse<-unlist(lapply(spltDat,"[",1))
         }
        
        values$Nplots<-min(input$numPlts,(ncol(values$dat)-1))^2+
          ifelse(input$showRespGam,min(input$numPlts,(ncol(values$dat)-1)),0)
        values$n.col<-ifelse(length(input$chkGrp)==0,input$numPlts,
                             min(length(input$chkGrp),input$numPlts))
        
        values$Nplots<-min(input$numPlts,(length(input$chkGrp)))^2+
          ifelse(input$showRespGam,min(input$numPlts,(length(input$chkGrp))),0)
     })
      
      NumPlots<-reactive({
        min(input$numPlts,(length(input$chkGrp)))^2+
          ifelse(input$showRespGam,min(input$numPlts,(length(input$chkGrp))),0)
      })
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
              
              colNum<-num %% (values$n.col+input$showRespGam)
              rowNum<-ceiling(num/(values$n.col+input$showRespGam))
              #modular arithmitic doesn't quite map how I need
              if(colNum==0) colNum<-values$n.col+input$showRespGam
              #use the zero column for the relationship bw resp and pred if it is to be used
              
              vtu<-c(names(values$dat)[names(values$dat)%in%values$varsToUse],"resp")
              Usedat<-values$dat[,names(values$dat)%in%vtu]
              
              if(input$showRespGam) colNum<-colNum-1 
             
              ggpairs(Usedat[,c(1:min(input$numPlts,(ncol(Usedat)-1)),ncol(Usedat))],
                      alph=input$alpha,pointSize=input$pointSize,DevScore=2,
                      brushRegion=XYs$brushRegion,
                      rowNum=rowNum,colNum=colNum)
              
            })
          })
        }
       
        n.row <- ceiling(Nplots/(values$n.col+input$showRespGam)) # calculate number of rows
        cnter <<- 0 # Counter variable
        rowStyle<-"padding: 1px;height:200,px;"
        # Create row with columns
        rows  <- lapply(1:n.row,function(row.num){
          cols  <- lapply(1:(values$n.col+input$showRespGam), function(i) {
            cnter    <<- cnter + 1
            brushName<-paste("plotbrush",cnter,sep="")
            plotname <- paste("plot", cnter, sep="")
            column(2, plotOutput(plotname,
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
        Yvar<-ceiling(as.numeric(plotNum)/(values$n.col+input$showRespGam))
        Xvar<-as.numeric(plotNum)%%(values$n.col+input$showRespGam)-input$showRespGam
        
        if (!is.null(brush)) {
          #three cases for the response look at the X and the correct response
          #for the hist the Yvar is wrong because it's a count
          
          #X and y are actually different for the first column
          if(Xvar==0) XYs$brushRegion<-values$dat[,Yvar]<=brush$xmax & values$dat[,Yvar]>=brush$xmin &
              values$dat[,ncol(values$dat)]<=brush$ymax & values$dat[,ncol(values$dat)]>=brush$ymin
          if(Xvar==Yvar) XYs$brushRegion<-values$dat[,Xvar]<=brush$xmax & values$dat[,Xvar]>=brush$xmin
          if(Xvar!=0 & Xvar!=Yvar) XYs$brushRegion<-values$dat[,Xvar]<=brush$xmax & 
              values$dat[,Xvar]>=brush$xmin &
              values$dat[,Yvar]<=brush$ymax & values$dat[,Yvar]>=brush$ymin
          
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
        o<-order(DevScore$devExp,decreasing=TRUE)
        choices<-paste(names(dat)[o],
                               " (",round(DevScore$devExp[o],digits=3),"%"," deviance explained)",sep="")
        checkboxGroupInput('chkGrp', 'Variables to include', choices=choices,selected=choices)
 
      })
      
     output$oneVarChoice <- renderUI({
       
        o<-order(DevScore$devExp,decreasing=TRUE)
        choices<-paste(names(dat)[o],
                       " (",round(DevScore$devExp[o],digits=3),"%"," deviance explained)",sep="")
       radioButtons('InptVar', 'Variable', choices=choices)
      })
  
     output$VarMap<-renderPlot({
        if(!is.null(input$InptVar)){
          InputVar<-strsplit(input$InptVar," ")[[1]][1]
          par(oma=c(0,0,0,0),mar=c(2,2,2,2))
         
          plot(layerStk, match(InputVar,names(layerStk)),cex.main=1.7,xlim=XYs$xlim,ylim=XYs$ylim,
               col=rev(inferno(25)),alpha=input$mpTr)
          #probably use a choice of maps here mpPtTr
          if(inherits(boundary,"SpatialPolygonsDataFrame")) 
             plot(boundary, add=TRUE,cex.main=3)
          if(!is.null(input$showTrain)){
           
            inBounds<-sdmdata[,2]>=XYs$ylim[1] & sdmdata[,2]<=XYs$ylim[2] & sdmdata[,1]>=XYs$xlim[1] & sdmdata[,1]<=XYs$xlim[2]
           
            if("showPres"%in%input$showTrain) 
                 points(sdmdata[(sdmdata[,3]==1 & inBounds),1],
                     sdmdata[(sdmdata[,3]==1 & inBounds),2], 
                     col=changeAlpha("deeppink",alpha=input$mpPtTr),
                     bg=changeAlpha("red",alpha=input$mpPtTr),pch=21,cex=input$mpPtSz)
            if("showAbs"%in%input$showTrain) 
                 points(sdmdata[(sdmdata[,3]==0 & inBounds),1],
                    sdmdata[(sdmdata[,3]==0 & inBounds),2],
                    col=changeAlpha("dodgerblue",alpha=input$mpPtTr), 
                    bg=changeAlpha("blue",alpha=input$mpPtTr),pch=21,cex=input$mpPtSz)
          } 
       }
      })
      
     output$Gam<-renderPlot({
      if(!is.null(input$InptVar)){
        
        InputVar<-strsplit(input$InptVar," ")[[1]][1]
        varNum<-match(InputVar,names(values$dat))
        brushRgn<-sdmdata[,2]>=XYs$ylim[1] & 
          sdmdata[,2]<=XYs$ylim[2] & 
          sdmdata[,1]>=XYs$xlim[1] & 
          sdmdata[,1]<=XYs$xlim[2]
        ggpairs(values$dat,alph=input$mpPtTr,pointSize=input$mpPtSz,DevScore=1,brushRegion=brushRgn,varNum,colNum=0)
      }
    })
     
     output$Hist<-renderPlot({
      if(!is.null(input$InptVar)){
        
        inBounds<-as.factor(c("Outside Subregion","Within Subregion")[
          factor(sdmdata[,2]>=XYs$ylim[1] & 
                      sdmdata[,2]<=XYs$ylim[2] & 
                      sdmdata[,1]>=XYs$xlim[1] & 
                      sdmdata[,1]<=XYs$xlim[2],levels=c("FALSE","TRUE"),ordered=TRUE)])
        InputVar<-strsplit(input$InptVar," ")[[1]][1]
        par(mfrow=c(1,length(table(inBounds))))
        colNum<-match(InputVar,names(values$dat))
        
        hstbounds<-hist(values$dat[,colNum],plot=FALSE)
        for(i in 1:length(table(inBounds))){
          hst<-hist(values$dat[names(table(inBounds))[i]==inBounds,colNum],
                   ylab="",xlab="",freq=TRUE,xlim=range(values$dat[,colNum],na.rm=TRUE),
                   ylim=c(0,max(hstbounds$counts)),
                   main=ifelse(length(table(inBounds))==1,
                               paste(names(values$dat)[colNum],"distribution"),
                               names(table(inBounds))[i]))
          rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
                 "grey89",border="grey89")
          hist(values$dat[names(table(inBounds))[i]==inBounds,colNum],breaks=hst$breaks,
                    add=TRUE,ylim=c(0,max(hstbounds$counts)),
                    col="red",
                    yaxt="n",ylab="",xlab="",freq=TRUE)
        hist(values$dat[(names(table(inBounds))[i]==inBounds & values$dat$resp==0),colNum],
              breaks=hst$breaks,add=TRUE,col="blue",freq=TRUE)
      
        }
      
      }
    })
      
    }
  )
           
runApp(app)   

}