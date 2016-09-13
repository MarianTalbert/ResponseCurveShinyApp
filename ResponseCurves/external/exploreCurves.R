exploreCurves<-function(fitLst,inputLayers,trainData,threshold=2,boundary=NA,testData){

cat("The interactive widget should come up momentarilly\n")
cat("Press escape to exit the interactive widget\n") 
   
 
Out<-formatModels(fitLst,inputLayers,trainData,threshold=threshold,testData)


rspHgt <- c("150px","300px","450px","650px","800px")[min(5,length(Out$modelLst))]
Cols <- c(wes_palette("Darjeeling"),wes_palette("GrandBudapest2"),wes_palette("Cavalcanti"),wes_palette("Moonrise3"))
cat("The interactive widget should come up momentarilly\n")
cat("Press escape to exit the interactive widget") 
evalPlotGroup=c("EvaluationMetrics","ROC","ConfusionMatrix","VariableImportance","Density")
#======================================================
#======================================================
app <- shinyApp(
  server=function(input, output) {
     
       XYs <- reactiveValues(
          Xlocs = NULL,
          Ylocs = NULL,
          xlim = c(xmin(layerStk),xmax(layerStk)),
          ylim = c(ymin(layerStk),ymax(layerStk)),
          vals= NULL
        )
      
      IntractVals<-reactiveValues(
      #start with the means
      Vals = vector()
           )
      
      observeEvent(input$resetVals,{
           IntractVals$Vals=vector()
      })
      
     
  #==============================================
  # Maps 
  #==========================
      # Handle clicks on the plot
      observeEvent(input$plotdblclick, {
          if (is.null(XYs$Xlocs)) {
            # We don't have a first click, so this is the first click
            XYs$Xlocs <- input$plotdblclick$x
            XYs$Ylocs <-  input$plotdblclick$y
          } else {
          XYs$Xlocs<-append(XYs$Xlocs,input$plotdblclick$x)
          XYs$Ylocs<-append(XYs$Ylocs,input$plotdblclick$y)
          }
           
            XYdat<-as.data.frame(cbind(X=XYs$Xlocs,Y=XYs$Ylocs))
            XYs$vals<-extract(layerStk,XYdat)
      })
      
      observeEvent(input$plotbrush,{
         brush <- input$plotbrush
          if (!is.null(brush)) {
            XYs$xlim <- c(brush$xmin, brush$xmax)
            XYs$ylim <- c(brush$ymin, brush$ymax)
            
          } else {
            XYs$xlim <- c(xmin(layerStk),xmax(layerStk))
            XYs$ylim <- c(ymin(layerStk),ymax(layerStk))
          }
      })
      observeEvent(input$resetNVals,{
           XYs$Xlocs=NULL
           XYs$Ylocs=NULL
           XYs$vals=NULL
      })
      observeEvent(input$resetExtent,{
        XYs$xlim <- c(xmin(layerStk),xmax(layerStk))
        XYs$ylim <- c(ymin(layerStk),ymax(layerStk))
      })
      #============================  
      #Map Generation
      lapply(1:length(Out$modelLst),function(i){
      output[[paste("map",i,sep="")]] <- renderPlot({
        interactiveMap(Out$predictedStk,Out$binaryStk,Out$messRast,Colors,Cols,input,i,boundary,
                       Out$Coords[[as.numeric(input$mapTestTrain)]],
                       Out$Stats,XYs,
                       resp=Out$resp[[as.numeric(input$mapTestTrain)]],
                       TestTrain=as.numeric(input$mapTestTrain))
        })
      })
      
      
      output$EnsembleMap <- renderPlot({
        #if(!missing(a)){ #if some other set chosen for ensemble update ensemble and ensemble binary
        #  Out$predictedStk[[length(Out$predictedStk)]]<-stackApply(predictedStk,indices=rep(1,times=length(fitLst)),fun=mean,na.rm=FALSE)
          #and do the same for the binary stack
        #}
        
        interactiveMap(Out$predictedStk,Out$binaryStk,Out$messRast,Colors,Cols,input,
                       (length(Out$modelLst)+1),boundary,Out$Coords[[as.numeric(input$mapTestTrain)]],
                       Out$Stats[[as.numeric(input$mapTestTrain)]],XYs,
                       resp=Out$resp[[as.numeric(input$mapTestTrain)]],
                       Ensemble=TRUE)
       
      })
   #============================    
   #Response Curve Generation for Map
   #============================ 
     
      lapply(1:length(Out$modelLst),function(i){
      output[[paste("curves",i,sep="")]] <- renderPlot({        
        #Plot the Curves BY MODEL
               responseCurves(fitLst,list(m=Out$modelLst[[i]]),vals=XYs$vals,
                              varIncluded=list(Out$varIncluded[[i]]),
                         varImp=list(Out$varImp[[i]][[as.numeric(input$mapTestTrain)]]),addImp=FALSE,
              dat=Out$dat[[1]],resp=Out$resp[[1]],Cols=Cols,Ensemble=FALSE,mapType=input$mapType,modelIdx=i,
              TestTrain=as.numeric(input$mapTestTrain))
        })
        })
      
      output$EnsemblePlot<-renderPlot({
        ensemebleCurves(fitLst,Out$modelLst,dat=Out$dat[[1]],Cols=Cols,XYs=XYs,
                        varIncluded=Out$varIncluded,
                        varImp=Out$varImp,mapType=input$mapType,TestTrain=as.numeric(input$mapTestTrain))
      })
      #Putting the clickable maps together here because the number plots is dynamic
      # Insert the right number of plot output objects into the web page
      output$ClickMaps <- renderUI({
        plot_output_list <- lapply(1:length(fitLst), function(i) {
          plotMap <- paste("map", i, sep="")
          plotCurves <- paste("curves", i, sep="")
         
          fluidRow(
            column(4,
                   wellPanel(
                     plotOutput(plotMap, dblclick = "plotdblclick",height="300px",
                                brush = brushOpts(
                                  id = "plotbrush",
                                  resetOnNew = TRUE
                                )),style="padding: 5px;"),style="padding: 5px;"),
            column(6,
                   wellPanel(plotOutput(plotCurves,height="300px"),style="padding: 5px;"),style="padding: 5px;" )
          )
        })
        
        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, plot_output_list)
      })
 #============================
 # Evaluation Plot
 #============================
     #eventually this should only use the plots selected but for now get rid of the check box
      #nEvalPlots<-reactive({ifelse(is.na(length(input$evalPlotGroup)),4,length(input$evalPlotGroup))})
       # IntractVals$nEvalPlots
   
      lapply(1:5,function(i){
        output[[paste("CalibPlot",i,sep="")]] <- renderPlot({
          Out$evaluationPlots[[1]][[i]]
        })
      })
      #browser()
      if(length(Out$evaluationPlots)==2){
        lapply(1:5,function(i){
          output[[paste("EvalPlot",i,sep="")]] <- renderPlot({
            Out$evaluationPlots[[2]][[i]]
          })
        })  
      }
       
        lapply(1:5,function(i){
          output[[paste("EvalTxt",i,sep="")]] <- renderText({ 
            switchEvalText(PlotType=evalPlotGroup[i])
                      })
                  })
  #==============================================
  # Sliders   
  #============================
      #Response curves for sliders
      
      observeEvent(input$addVals,{
        IntractV<-unlist(lapply(paste(Out$Variables,"aa",sep=""),FUN=function(l) input[[l]]))
        IntractVals$Vals<-rbind(IntractVals$Vals,IntractV)
       })
      
      lapply(1:length(Out$dataLst),IntractVals=IntractVals,function(i,IntractVals){
      output[[paste("slideRsp",i,sep="")]]<-renderPlot({
        responseCurves(fitLst,Out$modelLst,vals=IntractVals$Vals,i,varIncluded=Out$varIncluded,
                       varImp=Out$varImp,
                       addImp=input$addImp,dat=Out$dat[[1]],resp=Out$resp[[1]],Cols=Cols,
                       Ensemble=FALSE,TestTrain=as.numeric(input$mapTestTrain))
        })
      })
        
      #==============================================
      # Interactions   
      #============================  
      # predictor interaction
      output$sliders <- renderUI({
          
          f<-function(l){
          sliderInput(inputId=as.character(l$Name),label=as.character(l$Name),min=signif(l$min,digits=3),
                      max=signif(l$max,digits=3),value=signif(l$mean,digits=3),round=TRUE)
          }
          getNames<-function(x){as.character(x[[1]])}
          #we're not holding the predictors used in the surface constant so remove them from the
          #input slider list
          datNames<-unlist(lapply(Out$dataLst,getNames))
          match(c(input$FirstPredictor,input$SecondPredictor),Out$Variables)
         datForSliders<-Out$dataLst[-c(match(c(input$FirstPredictor,input$SecondPredictor),Out$Variables))]
         lapply(datForSliders, f)    
          })
      
      output$interact<-renderPlot({
       
       #get the value from the sliders using their position
      SlideNames<-Out$Variables[-c(which(Out$Variables%in%c(input$FirstPredictor,input$SecondPredictor)))]
      SlideVals<-unlist(lapply(SlideNames,FUN=function(l) input[[l]]))
          if(!is.null(SlideVals)){
              #slider values are missing the values for the indicies of the first and second predictor so put the spaces back in
              Svals<-vector(length=ncol(Out$dat[[1]]))
              toAdd<-sort(match(c(input$FirstPredictor,input$SecondPredictor),Out$Variables))
              datPos<-seq(1:ncol(Out$dat[[1]]))[-c(toAdd)]
              Svals[datPos]<-SlideVals
              SlideVals<-Svals
          }
      
      if(input$FirstPredictor==input$SecondPredictor){
       plot(0:1,0:1,type="n",xaxt="n",yaxt="n",xlab="",ylab="")   
      }else{if(input$Model=="All"){
        Dims<-ceiling(sqrt(length(Out$modelLst)))
        
        par(mfrow=c(Dims,Dims),mar=c(1,1,1,1),oma=c(0,0,0,0))
       
        for(i in 1:length(Out$modelLst)){
          interactionPlot(fitLst,Out$modelLst[[i]],vals=SlideVals,phi=input$phi,theta=input$theta,
                          x=input$FirstPredictor,y=input$SecondPredictor,Out$dat[[1]],Out$resp[[1]],
                          modelIndx=i)
          }
      } else{
       
         i<-match(input$Model,unlist(Out$modelLst))
          interactionPlot(fitLst,Out$modelLst[[i]],vals=Svals,phi=input$phi,theta=input$theta,
                          x=input$FirstPredictor,y=input$SecondPredictor,Out$dat[[1]],Out$resp[[1]],
                          modelIndx=i)
        }
      }  
      })
      #=====================
      # named sliders
      #creating a named list of sliders so I can put them where I feel like 
      lapply(1:length(Out$dataLst),function(i){
      output[[paste("slide",i,sep="")]] <- renderUI({ 
          sliderInput(inputId=paste(as.character(Out$dataLst[[i]]$Name),"aa",sep=""),
          label=as.character(Out$dataLst[[i]]$Name),min=signif(Out$dataLst[[i]]$min,digits=3),
          max=signif(Out$dataLst[[i]]$max,digits=3),
          value=signif(Out$dataLst[[i]]$mean,digits=3),round=TRUE)
          })
      })
      #=========================
      #a named list of predictor densities
      lapply(1:length(Out$dataLst),function(i,dat,resp){
      output[[paste("dens",i,sep="")]] <- renderPlot({
                 
                  presDens<-density(dat[resp==1,i])
                  absDens<-density(dat[resp==0,i])
                  par(mar=c(2,.3,0,.3),oma=c(0,0,0,0))
                  plot(x=range(c(absDens$x,presDens$x)),y=c(0,max(absDens$y,presDens$y)),type="n",
                  ylab="",xlab=Out$Variables[i],yaxt="n")
                  polygon(absDens,col=changeAlpha("blue",alpha=.5),border="blue")
                  polygon(presDens,col=changeAlpha("red",alpha=.5),border="red")
          })
      },dat=Out$dat[[as.numeric(input$mapTestTrain)]],
        resp=Out$resp[[as.numeric(input$mapTestTrain)]])      
      
      },      
ui=navbarPage("Respones Curve Explorer",
#=====================================
#===========UI========================
        #===============================================
        # ==========  Map Explorer ==========#
        tabPanel("Response Map Explorer",
        
        helpText("The response curves that are generated by holding each predictor constant at the mean",
           "are displayed in black.",
           "Clicking on any map pixel will generate a new response curve",
           "by holding all predictors except one constant at the value under the pixel rather than the mean",
           "while varying the remaining predictor across its range.",
           "This allows you to determine if the same factor is limiting the species at locations with similar predicted values,",
           "to determine which factor is causing the predicted value to differ in two locations\n\n",
           "or to identify areas where models disagree on the shape of the response curves where new data collection efforts should focus.",
           "The \"reset explorer\" button can be used to remove all points that have been selected.",
           "Ensembled Model results are shown in the last row.\n\n"),
           h4("Right clicking on a plot allows you to save an image to the image type you select"),
        fluidRow(
            column(1,
               radioButtons("mapType","Map Type:",
                     c("Probability"="mprob",
                       "Binary"="mbinary",
                       "MESS"="mess"))),  
            column(2,
              checkboxGroupInput("showTrain","Show Training Data",
              c("add presence points"="showPres",
              "add absence/background points"="showAbs")),
              checkboxInput("showResid","Add deviance residuals",value=FALSE)
              
                
              ),
            conditionalPanel(!missing(testData),
                column(2,
                   radioButtons("mapTestTrain","Deviance Residuals for Calibration or evaluation data:",
                             c("Calibration"=1,"Evaluation"=2,"Both"=3),selected=1)
                )
            ),
            
            column(2,
              actionButton("resetNVals", label = "Reset explorer"),
              actionButton("resetExtent",label = "Reset spatial extent" )
              )
        ),
       
        conditionalPanel(TRUE,
                         fluidRow(
                           column(4,
                                  wellPanel(
                                    plotOutput("EnsembleMap", dblclick = "plotdblclick",height="300px",
                                      brush = brushOpts(
                                      id = "plotbrush",
                                      resetOnNew = TRUE
                                    )),style="padding: 5px;"),style="padding: 5px;"),
                           column(6,
                                  wellPanel(plotOutput("EnsemblePlot",height="300px"),style="padding: 5px;"),style="padding: 5px;" )
                           
                         )
        ),
        uiOutput("ClickMaps")),
  #===============================================
  # ==========  Model Evaluation ==========#
        tabPanel("Model Evaluation",
        
         fluidRow(
            column(5,
                wellPanel(
                       plotOutput("CalibPlot1"),style="padding: 5px;", height="350px",
                       style="padding: 5px;")),
            conditionalPanel(length(Out$evaluationPlots)==2,
              column(5,wellPanel(
                plotOutput("EvalPlot1"),style="padding: 5px;", height="350px")
            ))),
         fluidRow(textOutput("EvalTxt1")),
         
         fluidRow(
           column(5,
                  wellPanel(
                    plotOutput("CalibPlot2"),style="padding: 5px;", height="350px",
                    style="padding: 5px;")),
           conditionalPanel(length(Out$evaluationPlots)==2,
                            column(5,wellPanel(
                              plotOutput("EvalPlot2"),style="padding: 5px;", height="350px")
                            ))),
         fluidRow(textOutput("EvalTxt2")),
                   
             
         fluidRow(
           column(5,
                  wellPanel(
                    plotOutput("CalibPlot3"),style="padding: 5px;", height="350px",
                    style="padding: 5px;")),
           conditionalPanel(length(Out$evaluationPlots)==2,
                            column(5,wellPanel(
                              plotOutput("EvalPlot3"),style="padding: 5px;", height="350px")
                            ))),
         fluidRow(textOutput("EvalTxt3")),
         fluidRow(
           column(5,
                  wellPanel(
                    plotOutput("CalibPlot4"),style="padding: 5px;", height="350px",
                    style="padding: 5px;")),
           conditionalPanel(length(Out$evaluationPlots)==2,
                            column(5,wellPanel(
                              plotOutput("EvalPlot4"),style="padding: 5px;", height="350px")
                            ))),
         fluidRow(textOutput("EvalTxt4")),
         fluidRow(
           column(5,
                  wellPanel(
                    plotOutput("CalibPlot5"),style="padding: 5px;", height="350px",
                    style="padding: 5px;")),
           conditionalPanel(length(Out$evaluationPlots)==2,
                            column(5,wellPanel(
                              plotOutput("EvalPlot5"),style="padding: 5px;", height="350px")
                            ))),
         fluidRow(textOutput("EvalTxt5"))
          
                ),
        #===============================================
        # ==========  Slide Explorer ==========#
        tabPanel("Slide Explorer",
        helpText("Use the sliders to change the value that each predictor is held at in generating the response", 
                 "curves once you are satisfied with your selections clicking \"add curves for current values\" will",
                 "add these values in a new color to the plot.  This can be used to determine how the response space",
                 "changes when certian predictors are held at values that are thought to be important to the species."), 
        fluidRow(
        column(3,  
          checkboxInput("addImp", label = "show variable importance with background color",value=FALSE)),
        column(3,
          actionButton("addVals", label = "add curves for current values")),
        column(2,  
          actionButton("resetVals", label = "Reset explorer"))
        ),
        
        fluidRow(
        lapply(1:length(Out$Variables),function(i){
        column(1,uiOutput(paste0("slide",i)))
        })),
        fluidRow(
        lapply(1:length(Out$Variables),function(i){
        column(1,plotOutput(paste0("slideRsp",i),height=rspHgt))
        }))
        
        ),
        
        #===============================================
        # ==========  Interaction ==========#
        tabPanel("Interaction Tool",
        helpText("Explore the interaction between two predictors in the model while holding",
           "all other predictors constant at the value",
           "specified by the sliders"),
        sidebarPanel(  
                    uiOutput("sliders"),                           
                    width=2),                           
        mainPanel(
          fluidRow(
            column(3,
                selectInput("FirstPredictor", choices=Out$Variables,
                  selected=Out$Variables[1],label=h4("First Predictor"))),
            column(3,    
                selectInput("SecondPredictor", choices=Out$Variables,
                   selected=Out$Variables[max(length(Out$Variables),2)],label=h4("Second Predictor"))),
            column(3,    
                selectInput("Model", choices=c("All",unlist(Out$modelLst)),label=h4("Model")))
           ),     
          wellPanel(
              plotOutput("interact"),
               sliderInput("phi", label = h4("phi"),
                                                min = 0, max = 90,value=35),
               sliderInput("theta", label = h4("theta"),
                                                min = 0, max = 360,value=120),
              width="100%",height="120%")                               
        )
        )
        
        
        ))
        #  This is where the magic happens
runApp(app)
}



