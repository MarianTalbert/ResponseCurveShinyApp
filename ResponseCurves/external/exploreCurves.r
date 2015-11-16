exploreCurves<-function(fitLst,inputLayers,data,threshold=2,boundary=NA,Ensemble=FALSE){

cat("The interactive widget should come up momentarilly\n")
cat("Press escape to exit the interactive widget\n") 
    #putting together all of the global input needed by both the server and ui fcts
    predictedStk<-varImp<-predictedVals<-binaryVals<-varIncluded<-Thresh<-binaryStk<-Stats<-cmxPlot<-list()
    
    modelLst<-names(fitLst)
    PresCoords<-data[data[,3]==1,c(1,2)]
    AbsCoords<-data[data[,3]==0,c(1,2)]
   
    resp<-data[,3]
    dat<-data[,-c(1:3)]
    Variables<-names(dat)
    Split<-seq(1:length(resp))
    
    for(i in 1:length(fitLst)){
          predictedVals[[i]]<-predictBinary(fitLst[[i]],dat)
          if(inherits(fitLst[[i]],"randomForest")){
            #because of the distinction between oob prediction and in bag prediction
            #we end up with two sets of predicted vals for rf 
            inBagResp<-predict(fitLst[[i]],dat,type='response')
            AUCVal<-roc(resp,inBagResp)
          }else{
            AUCVal<-roc(resp,predictedVals[[i]])
          }
          Thresh[[i]]<-optimal.thresholds(DATA=cbind(seq(1:nrow(dat)),resp,predictedVals[[i]]))[2,threshold]
          binaryVals<-as.numeric(predictedVals[[i]]>=Thresh[[i]])
          
           Stats[[i]]<-calcStat(predictedVals[[i]],resp,Split,Thresh[[i]])
           if(i==1){ 
             predictedStk<-predict(inputLayers,fitLst[[i]],type='response')
             binaryStk<-createBinary(predictedStk[[i]],Thresh[[i]])
             messRast<-mess(inputLayers,dat,full=FALSE) 
           }else{ 
             predictedStk<-addLayer(predictedStk,predict(inputLayers,fitLst[[i]],type='response'))
             binaryStk<-addLayer(binaryStk,createBinary(predictedStk[[i]],Thresh[[i]]))
           }
        Permuted<-permutePredict(dat,fitLst[[i]],resp)
        varIncluded[[i]]<-Permuted$varIncluded  
        varImp[[i]]<-AUCVal-Permuted$AUC
        M<-data.frame(Percent=c(100*Stats[[i]]$Cmx[2]/sum(Stats[[i]]$Cmx[1:2]),100*Stats[[i]]$Cmx[4]/sum(Stats[[i]]$Cmx[3:4]),
                                100*Stats[[i]]$Cmx[1]/sum(Stats[[i]]$Cmx[1:2]),100*Stats[[i]]$Cmx[3]/sum(Stats[[i]]$Cmx[3:4])),
                      Predicted=factor(c("Absence","Absence","Presence","Presence")),
                      Observed=factor(c("Presence","Absence","Presence","Absence")),Model=rep(names(fitLst)[i],times=4))
        
        if(i==1) Mdat <- M
        else Mdat<-rbind(Mdat,M)
    }
    
    densityFrame<-data.frame(Predicted=unlist(predictedVals),Response=rep(resp,times=length(fitLst)),
                             Model=rep(names(fitLst),each=length(resp)))
    densityFrame$Model<-as.factor(densityFrame$Model)
    densityFrame$Response<-as.factor(densityFrame$Response)
    
       if(Ensemble){
      EnsemblePred<-stackApply(predictedStk,indices=rep(1,times=length(fitLst)),fun=mean)
      EnsembleBin<-stackApply(binaryStk,indices=rep(1,times=length(fitLst)),fun=sum)
      names(EnsemblePred)<-"Ensemble (Mean) of probability maps"
      names(EnsembleBin)<-"Ensemble (Sum) of binary maps"
    }
    
    pre<-cbind(seq(1:length(resp)),resp,matrix(unlist(predictedVals),ncol=4))
    
   
    
    names(predictedStk)<-names(fitLst)
    names(binaryStk)<-names(fitLst)
    
   #I'M ASSUMING FOR NOW CONSISTENT VARIABLES ACROSS MODELS
   varImpMat<-data.frame(VariableImportance=unlist(varImp),
                         Variable=as.factor(rep(names(dat),times=length(modelLst))),
                         Model=as.factor(rep(names(fitLst),each=length(varImp))))
       Statistics = unlist(lapply(Stats,FUN=function(x){x[c(1,3,4,5,6)]}))
      StatsFrame = data.frame(Stat=as.factor(names(Statistics)),Value=as.vector(Statistics),Model=as.factor(rep(names(fitLst),each=5)))
     
         
    d=data.frame(Name=names(dat),min=apply(dat,2,min,na.rm=TRUE),
   max <-apply(dat,2,max,na.rm=TRUE),mean=apply(dat,2,mean,na.rm=TRUE))
  dataLst <- split(d,f=seq(1:nrow(d)))
   
max_plots <- 5
rspHgt <- c("150px","300px","450px","650px")[length(fitLst)]
Cols <- c(wes_palette("Darjeeling"),wes_palette("GrandBudapest2"),wes_palette("Cavalcanti"),wes_palette("Moonrise3"))
cat("The interactive widget should come up momentarilly\n")
cat("Press escape to exit the interactive widget") 
cexMult <- 1.5
evalPlotGroup=c("EvaluationMetrics","ROC","ConfusionMatrix","VariableImportance","Density")
modelNames<-names(fitLst)
#======================================================
#======================================================
app <- shinyApp(
  server=function(input, output) {
     
       XYs <- reactiveValues(
          Xlocs = NULL,
          Ylocs = NULL,
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
      observeEvent(input$plot_click, {
          if (is.null(XYs$Xlocs)) {
            # We don't have a first click, so this is the first click
            XYs$Xlocs <- input$plot_click$x
            XYs$Ylocs <-  input$plot_click$y
          } else {
          XYs$Xlocs<-append(XYs$Xlocs,input$plot_click$x)
          XYs$Ylocs<-append(XYs$Ylocs,input$plot_click$y)
          }
           
            XYdat<-as.data.frame(cbind(X=XYs$Xlocs,Y=XYs$Ylocs))
            XYs$vals<-extract(layerStk,XYdat)
      })
      observeEvent(input$resetNVals,{
           XYs$Xlocs=NULL
           XYs$Ylocs=NULL
           XYs$vals=NULL
      })
      #============================  
      #Map Generation
      lapply(1:length(modelLst),function(i){
      output[[paste("map",i,sep="")]] <- renderPlot({       
        interactiveMap(predictedStk,binaryStk,messRast,Colors,Cols,input,i,boundary,data,Stats,XYs,
                       PresCoords,AbseCoords)
        })
      })
      
      output$EnsembleMap <- renderPlot({
        interactiveMap(EnsemblePred,EnsembleBin,messRast,Colors,Cols,input,1,boundary,data,Stats,XYs,
                       PresCoords,AbseCoords,Ensemble=TRUE)
       
      })
   #============================    
   #Response Curve Generation for Map
   #============================ 
     
      lapply(1:length(modelLst),function(i){
      output[[paste("curves",i,sep="")]] <- renderPlot({        
        #Plot the Curves
          responseCurves(list(f=fitLst[[i]]),list(m=modelLst[[i]]),vals=XYs$vals,varIncluded=list(varIncluded[[i]]),varImp=list(varImp[[i]]),addImp=input$addMImp,
              dat=dat,resp=resp,Cols=Cols,Ensemble=FALSE)
        })
        })
      
      output$EnsemblePlot<-renderPlot({
        ensemebleCurves(fitLst,modelLst,dat=dat,Cols=Cols,XYs=XYs,varIncluded=varIncluded,varImp=varImp)
      })
 #============================
 # Evaluation Plot
 #============================
     #eventually this should only use the plots selected but for now get rid of the check box
      #nEvalPlots<-reactive({ifelse(is.na(length(input$evalPlotGroup)),4,length(input$evalPlotGroup))})
       # IntractVals$nEvalPlots
   
      lapply(1:5,function(i){
        output[[paste("EvalPlot",i,sep="")]] <- renderPlot({
           switchEvalPlots(PlotType=evalPlotGroup[i],StatsFrame,pre,Thresh,
                          Mdat,varImpMat,densityFrame,modelNames,cexMult)
        })
      })
      
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
        IntractV<-unlist(lapply(paste(names(dat),"aa",sep=""),FUN=function(l) input[[l]]))
        IntractVals$Vals<-rbind(IntractVals$Vals,IntractV)
       })
      
      lapply(1:length(dataLst),IntractVals=IntractVals,function(i,IntractVals){
      output[[paste("slideRsp",i,sep="")]]<-renderPlot({
        responseCurves(fitLst,modelLst,vals=IntractVals$Vals,i,varIncluded=varIncluded,varImp=varImp,
                       addImp=input$addImp,dat=dat,resp=resp,Cols=Cols,Ensemble=FALSE)
        })
      })
        
      #==============================================
      # Interactions   
      #============================  
      # predictor interaction
      output$sliders <- renderUI({
          
          f<-function(l){
          sliderInput(inputId=as.character(l$Name),label=as.character(l$Name),min=signif(l$min,digits=3),max=signif(l$max,digits=3),value=signif(l$mean,digits=3),round=TRUE)
          }
          getNames<-function(x){as.character(x[[1]])}
          #we're not holding the predictors used in the surface constant so remove them from the
          #input slider list
          datNames<-unlist(lapply(dataLst,getNames))
          match(c(input$FirstPredictor,input$SecondPredictor),Variables)
         datForSliders<-dataLst[-c(match(c(input$FirstPredictor,input$SecondPredictor),Variables))]
         lapply(datForSliders, f)    
          })
      
      output$interact<-renderPlot({
       
       #get the value from the sliders using their position
      SlideNames<-names(dat)[-c(which(names(dat)%in%c(input$FirstPredictor,input$SecondPredictor)))]
      SlideVals<-unlist(lapply(SlideNames,FUN=function(l) input[[l]]))
          if(!is.null(SlideVals)){
              #slider values are missing the values for the indicies of the first and second predictor so put the spaces back in
              Svals<-vector(length=ncol(dat))
              toAdd<-sort(match(c(input$FirstPredictor,input$SecondPredictor),names(dat)))
              datPos<-seq(1:ncol(dat))[-c(toAdd)]
              Svals[datPos]<-SlideVals
              SlideVals<-Svals
          }
      if(input$FirstPredictor==input$SecondPredictor){
       plot(0:1,0:1,type="n",xaxt="n",yaxt="n",xlab="",ylab="")   
      }else{if(input$Model=="All"){
        par(mfrow=c(2,2),mar=c(1,1,1,1),oma=c(0,0,0,0))
        for(i in 1:length(fitLst)){
          interactionPlot(fitLst[[i]],modelLst[[i]],vals=SlideVals,phi=input$phi,theta=input$theta,x=input$FirstPredictor,y=input$SecondPredictor,dat,resp)
          }
      } else{
         i<-match(input$Model,unlist(modelLst))
          interactionPlot(fitLst[[i]],modelLst[[i]],vals=Svals,phi=input$phi,theta=input$theta,x=input$FirstPredictor,y=input$SecondPredictor,dat,resp)
        }
      }  
      })
      #=====================
      # named sliders
      #creating a named list of sliders so I can put them where I feel like 
      lapply(1:length(dataLst),function(i){
      output[[paste("slide",i,sep="")]] <- renderUI({ 
          sliderInput(inputId=paste(as.character(dataLst[[i]]$Name),"aa",sep=""),
          label=as.character(dataLst[[i]]$Name),min=signif(dataLst[[i]]$min,digits=3),
          max=signif(dataLst[[i]]$max,digits=3),
          value=signif(dataLst[[i]]$mean,digits=3),round=TRUE)
          })
      })
      #=========================
      #a named list of predictor densities
      lapply(1:length(dataLst),function(i,dat,resp){
      output[[paste("dens",i,sep="")]] <- renderPlot({
                 cols<-c("blue","red")
                color.box<-col2rgb(cols,alpha=TRUE)
                                 color.box[4,]<-60
                temp.fct<-function(a){return(rgb(red=a[1],green=a[2],blue=a[3],alpha=a[4]))}
                  
                cols<-apply(color.box/255,2,temp.fct)
                  presDens<-density(dat[resp==1,i])
                  absDens<-density(dat[resp==0,i])
                  par(mar=c(2,.3,0,.3),oma=c(0,0,0,0))
                  plot(x=range(c(absDens$x,presDens$x)),y=c(0,max(absDens$y,presDens$y)),type="n",
                  ylab="",xlab=names(dat)[i],yaxt="n")
                  polygon(absDens,col=cols[1],border="blue")
                  polygon(presDens,col=cols[2],border="red")
          })
      },dat=dat,resp=resp)      
      
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
              checkboxInput("showResid","Add deviance residuals",value=FALSE)),
              
            
            column(2,
              checkboxInput("addMImp", label = "show variable importance with background color",value=FALSE),
              actionButton("resetNVals", label = "Reset explorer")
              )
        ),
        
        # Insert the right number of plot output objects into the web page
        fluidRow(
          column(4,
          wellPanel(
          plotOutput("map1", click = "plot_click",height="350px"),style="padding: 5px;"),style="padding: 5px;"),
          column(6,
          wellPanel(plotOutput("curves1",height="350px"),style="padding: 5px;"),style="padding: 5px;" )
          ),
        conditionalPanel(length(modelLst)>1,
         fluidRow(
          column(4,
          wellPanel(
          plotOutput("map2", click = "plot_click",height="350px"),style="padding: 5px;"),style="padding: 5px;"),
          column(6,
          wellPanel(plotOutput("curves2",height="350px"),style="padding: 5px;"),style="padding: 5px;" )
          )
         ),
         conditionalPanel(length(modelLst)>2,
         fluidRow(
          column(4,
          wellPanel(
          plotOutput("map3", click = "plot_click",height="350px"),style="padding: 5px;"),style="padding: 5px;"),
          column(6,
          wellPanel(plotOutput("curves3",height="350px"),style="padding: 5px;"),style="padding: 5px;" )
          )
         ),
         conditionalPanel(length(modelLst)>3,
         fluidRow(
          column(4,
          wellPanel(
          plotOutput("map4", click = "plot_click",height="350px"),style="padding: 5px;"),style="padding: 5px;"),
          column(6,
          wellPanel(plotOutput("curves4",height="350px"),style="padding: 5px;"),style="padding: 5px;" )
          
          )
          ),
        conditionalPanel(length(modelLst)>3,
                         fluidRow(
                           column(4,
                                  wellPanel(
                                    plotOutput("EnsembleMap", click = "plot_click",height="350px"),style="padding: 5px;"),style="padding: 5px;"),
                           column(6,
                                  wellPanel(plotOutput("EnsemblePlot",height="350px"),style="padding: 5px;"),style="padding: 5px;" )
                           
                         )
        )
        
        ),
  #===============================================
  # ==========  Model Evaluation ==========#
        tabPanel("Model Evaluation",
                 
         fluidRow(
            column(5,
                wellPanel(
                       plotOutput("EvalPlot1"),style="padding: 5px;", height="400px"),
                textOutput("EvalTxt1"),
                       style="padding: 5px;"),
              column(5,
                wellPanel(
                       plotOutput("EvalPlot2"),style="padding: 5px;", height="400px"),
                       textOutput("EvalTxt2"), 
                       style="padding: 5px;")         
              ),
            fluidRow(           
             column(5,
             wellPanel(
               plotOutput("EvalPlot3", height="350px"),style="padding: 5px;"),
             textOutput("EvalTxt3"), 
             style="padding: 5px;"),
             fluidRow(           
               column(5,
                      wellPanel(
                        plotOutput("EvalPlot4", height="350px"),style="padding: 5px;"),
                      textOutput("EvalTxt4"), 
                      style="padding: 5px;")
             ),
             fluidRow(           
               column(5,
                      wellPanel(
               plotOutput("EvalPlot5", height="350px"),style="padding: 5px;"),
               textOutput("EvalTxt5"), 
               style="padding: 5px;")
           )
        )
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
        lapply(1:length(Variables),function(i){
        column(1,uiOutput(paste0("slide",i)))
        })),
        fluidRow(
        lapply(1:length(Variables),function(i){
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
                selectInput("FirstPredictor", choices=Variables,
                  selected=Variables[1],label=h4("First Predictor"))),
            column(3,    
                selectInput("SecondPredictor", choices=Variables,
                   selected=Variables[max(length(Variables),2)],label=h4("Second Predictor"))),
            column(3,    
                selectInput("Model", choices=c("All",unlist(modelLst)),label=h4("Model")))
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



"roc" <-
function (obsdat, preddat)
{
# code adapted from Ferrier, Pearce and Watson's code, by J.Elith
#
# see:
# Hanley, J.A. & McNeil, B.J. (1982) The meaning and use of the area
# under a Receiver Operating Characteristic (ROC) curve.
# Radiology, 143, 29-36
#
# Pearce, J. & Ferrier, S. (2000) Evaluating the predictive performance
# of habitat models developed using logistic regression.
# Ecological Modelling, 133, 225-245.
# this is the non-parametric calculation for area under the ROC curve,
# using the fact that a MannWhitney U statistic is closely related to
# the area
#
    if (length(obsdat) != length(preddat))
        stop("obs and preds must be equal lengths")
    n.x <- length(obsdat[obsdat == 0])
    n.y <- length(obsdat[obsdat == 1])
    xy <- c(preddat[obsdat == 0], preddat[obsdat == 1])
    rnk <- rank(xy)
    wilc <- ((n.x * n.y) + ((n.x * (n.x + 1))/2) - sum(rnk[1:n.x]))/(n.x *
        n.y)
    return(round(wilc, 4))
}