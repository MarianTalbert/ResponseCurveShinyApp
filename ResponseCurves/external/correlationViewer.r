correlationViewer<-function(data,num.plots=5,min.cor=.7,input.file,output.file,response.col="ResponseBinary",cors.w.highest=FALSE){              
  #Remove coordinates, response column, site.weights
  #before exploring predictor relationship COLUMNS 
 
  famly<-"binomial"    
  missing.summary<-1-apply(apply(data,2,complete.cases),2,sum)/nrow(data)
  response<-data[,3]
  data<-data[,-c(1:3)]
  Variables<-names(data)
  #subsample the data so we can calculate correlations quickly
     response.table<-table(response)
     max.points<-1500
     if(any(response.table> max.points)){
       for(i in names(response.table[response.table> max.points])){
             s<-sample(which(response==i,arr.ind=TRUE),size=(sum(response==i)- max.points))
             data<-data[-c(s),]
             response<-response[-c(s)]
       }
     }   
      
       #rm.cols<-unique(c(rm.cols,which(include==0,arr.ind=TRUE)))
       # data<-data[,-rm.cols]
        
      devExp<-vector()
       for(i in (1:ncol(data))){
            devExp[i]<-try(my.panel.smooth(data[,i], response,plot.it=FALSE,famly=famly),silent=TRUE)
           }
          #after calculating the deviance for all predictors we have to remove the excluded predictors for the following plots
    
  
  #Remove columns with only one unique value
    data<-try(data[,as.vector(apply(data,2,var,na.rm=TRUE)==0)!=1],silent=TRUE)
    if(class(data)=="try-error") stop("data frame contains nonnumeric columns please remove and continue")
  
  #record correlations for later plots

    cmat<-cor(data,use="pairwise.complete.obs")
    smat<-cor(data,method="spearman",use="pairwise.complete.obs")
    if(dim(data)[1]<2000){
    kmat<-cor(data,method="kendall",use="pairwise.complete.obs")}
    else {s<-sample(seq(1:dim(data)[1]),size=2000,replace=FALSE)
     kmat<-cor(data[s,],method="kendall",use="pairwise.complete.obs")
    }
    cmat=pmax(abs(cmat),abs(smat),abs(kmat),na.rm=TRUE)
    cmat[is.na(cmat)]<-0
    High.cor<-sort(apply(abs(cmat)>min.cor,2,sum)-1,decreasing=TRUE)

  #take the top num.plots to put in the pairs plot or if the looking at a single
  #predictor and other predictors it's correlated with, take the top num.plots-1
  #of those with which it is correlated
    if(cors.w.highest==TRUE){
          #take the column of the correlation matrix corresponding to the
          #predictor with the higest number of total correlations record the names
          #of the predictors that are correlated with this one predictor
          temp<-cmat[rownames(cmat)==names(High.cor[1]),]
          CorWHigh<-temp[abs(cmat[,colnames(cmat)==names(High.cor[1])])>min.cor]

          #record counts of total number of correlations with all predictors for those
          #predictors that are highly correlated with the Highest predictor
          High.cor<-sort(High.cor[names(CorWHigh)],decreasing=TRUE)    
     }
     HighToPlot<-data[,match(names(High.cor),names(data))[1:min(num.plots,length(High.cor))]]
     #Data<-data[,match(names(High.cor),names(data))[1:min(num.plots,length(High.cor))]]
     
    cor.hightoplot<-abs(cor(HighToPlot,use="pairwise.complete.obs"))
    diag(cor.hightoplot)<-0
    cor.hightoplot[is.na(cor.hightoplot)]<-0 
    cor.range<-c(quantile(as.vector(cor.hightoplot),probs=c(0,.5,.7,.85)),1)
     
     missing.summary<-missing.summary[match(names(High.cor),names(missing.summary))[1:min(num.plots,length(High.cor))]]
     
 
 options(warn=-1)
 num.plots<-min(ncol(HighToPlot),num.plots)
 if(num.plots<8) {wdth=1500
                 cex.mult=2}
 else if(num.plots<15) {wdth=3000
                        if(num.plots<12) cex.mult=4
                        else cex.mult=3
                          }
      else {wdth=4500
      if(num.plots<17) cex.mult=4
                        else cex.mult=3
      }
 
  #Find a new unique file name (one in the desired directory that hasn't yet been used)
  MyPairs(cbind(response,HighToPlot),cor.range=cor.range,missing.summary=missing.summary,my.labels=(as.vector(High.cor)[1:num.plots]),
        lower.panel=panel.smooth,diag.panel=panel.hist, upper.panel=panel.cor,pch=21,
        bg = c("blue","red")[factor(response,levels=c(0,1))],col.smooth = "red",cex.mult=cex.mult,oma=c(0,2,6,0),famly=famly)
#maybe put in app eventually                   
app <- shinyApp(
    ui = fluidPage(
      numericInput("Number of Plots to display", "n", 1),
      plotOutput("parisPlot",height=1000,width=1000)
    ),
    server = function(input, output) {
      output$parisPlot <- renderPlot(
       MyPairs(cbind(response,HighToPlot),cor.range=cor.range,missing.summary=missing.summary,my.labels=(as.vector(High.cor)[1:num.plots]),
        lower.panel=panel.smooth,diag.panel=panel.hist, upper.panel=panel.cor,pch=21,
        bg = c("blue","red")[factor(response,levels=c(0,1))],col.smooth = "red",cex.mult=cex.mult,oma=c(0,2,6,0),famly=famly)
       )
    }
  )
           
#runApp(app)   



}