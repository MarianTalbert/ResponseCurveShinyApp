ggpairs<-function(dat,alph,pointSize,DevScore,showResp,brushRegion,rowNum,colNum){
  
  color.box<-col2rgb(c("#08519C","#CB181D","blue4","deeppink4"),alpha=TRUE)
  color.box[4,]<-255*alph
  temp.fct<-function(a){return(rgb(red=a[1],green=a[2],blue=a[3],alpha=a[4]))}
  Cols<-apply(color.box/255,2,temp.fct)
 
 
  dat$brushResp<-factor(paste(c("nonbrush","brush")[as.factor(brushRegion)],
                                 dat[,ncol(dat)],sep="."),
                                 levels=c("nonbrush.0","nonbrush.1","brush.0","brush.1"))
  respCol<-ncol(dat)-1
 
  d<-data.frame(x=c(0,1),y=c(0,1))
  colOffset<-ifelse(showResp,1,0) 
  dat[,respCol]<-as.numeric(as.character(dat[,respCol])) 
  par(mar=c(2,2,0,0))
#===================
  #response column
  if(colNum==0){
  
    dat[,respCol]<-as.numeric(dat[,respCol]) 
  
     respPlt<-ggplot(dat, aes_q(x = as.name(names(dat)[rowNum]), 
                               y =as.name(names(dat)[respCol]))) + 
      geom_point(aes_q(x = as.name(names(dat)[rowNum]), 
                       y =as.name(names(dat)[respCol]),
                       colour=as.name(names(dat)[respCol+1])),alpha=alph) +
      stat_smooth(method="glm", method.args=list(family="binomial"), formula = y ~ ns(x, 3))+
      scale_colour_manual(values = Cols)+theme(legend.position="none")+
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),plot.margin=unit(c(0,1,1,0),"mm"))+
      xlab("")+ylab("")
    
    if(rowNum==1) respPlt<-respPlt+ggtitle("Response")
    return(respPlt)
}
  dat[,respCol]<-as.factor(dat[,respCol]) 
  
  #================================
  #histogram above the diagonal
  if(rowNum==colNum){ 
    #might want to think about two colors for pres and absence at some point
    hist(dat[,2],main=ifelse(rowNum==1,names(dat[rowNum]),""),yaxt="n",col="blue",ylab="",xlab="")
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
           "grey89",border="grey89")
    hist(dat[,2],yaxt="n",col="blue",add=TRUE,ylab="")
     
    return()
  }  
  #================================  
  #pairs plot below the diagonal

  if(rowNum>colNum){
    
    plot(x=dat[,colNum],y=dat[,rowNum],type="n",
         xaxt=ifelse(rowNum!=(respCol-1),"n","s"),
         ylab=ifelse(rowNum!=(respCol-1),"",names(dat)[rowNum]),
         yaxt=ifelse(colNum!=1,"n","s"),xlab=ifelse(colNum!=1,"",names(dat)[colNum]))
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
           "grey89",border="grey89")
    points(x=dat[,colNum],y=dat[,rowNum],col=Cols[dat$brushResp],pch=16,
           cex=c(pointSize,1.6*pointSize)[factor(brushRegion)],bty="n")
            return()
    }  
  #=============================
  #color above the diagonal
  if(rowNum<colNum){ 
   
    Cols<-c("white",brewer.pal(9,"Reds"))
        Cor<-cor(dat[,rowNum],dat[,colNum])
        ColIndx<-cut(abs(Cor),breaks=c(0,seq(from=.6,to=1,length=length(Cols))))
        colPlot<-ggplot(d,  aes(x = x, y = y))+ geom_blank()+
          theme(panel.background = element_rect(fill = Cols[ColIndx]))+
          theme(panel.grid.major = element_line(colour = Cols[ColIndx]))+
          theme(panel.grid.minor = element_line(colour = Cols[ColIndx]),
                plot.margin=unit(c(0,1,1,0),"mm"))+
          ylab("")+xlab("")+scale_x_continuous(breaks=NULL)+
          scale_y_continuous(breaks=NULL)+theme(axis.title=element_text(size=rel(1.3)))+
          annotate("text", label= round(Cor,digits=2), x=.5, y=.5,
                   size=15*abs(Cor))
        if(rowNum==1) colPlot<-colPlot+ggtitle(names(dat)[colNum])
        return(colPlot)
  }  
}

