ggpairs<-function(dat,alph,pointSize,DevScore,showResp,brushRegion,rowNum,colNum){
  
  origCols<-c("blue","red","blue","red")
  if(any(brushRegion)) origCols[c(1,2)]<-c("lightskyblue3","rosybrown1")
  Cols<-changeAlpha(origCols,alpha=alph)
  
  dat$brushResp<-factor(paste(c("nonbrush","brush")[as.factor(brushRegion)],
                                 dat[,ncol(dat)],sep="."),
                                 levels=c("nonbrush.0","nonbrush.1","brush.0","brush.1"))
  respCol<-ncol(dat)-1
  dat$brush<-as.factor(brushRegion)
  d<-data.frame(x=c(0,1),y=c(0,1))
  colOffset<-ifelse(showResp,1,0) 
  dat[,respCol]<-as.numeric(as.character(dat[,respCol])) 
  par(mar=c(2,ifelse(colNum==1,1,0),ifelse(rowNum==1,1,0),0))
 
#===================
  #response column
  if(colNum==0){
   
    dat[,respCol]<-as.numeric(dat[,respCol]) 
 
     respPlt<-ggplot(dat, aes_q(x = as.name(names(dat)[rowNum]), 
                               y =as.name(names(dat)[respCol]))) + 
      geom_point(aes_q(x = as.name(names(dat)[rowNum]), 
                       y =as.name(names(dat)[respCol]),
                       colour=as.name(names(dat)[respCol+1])),size=c(1,2)[as.factor(brushRegion)],alpha=alph) +
      stat_smooth(method="glm", method.args=list(family="binomial"), formula = y ~ ns(x, 3))+
      scale_colour_manual(values = Cols[which(table(dat[,respCol+1])!=0)])+theme(legend.position="none")+
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
    
    hst<-hist(dat[,colNum],col=origCols[1],main=ifelse(rowNum==1,names(dat[rowNum]),""),
              yaxt="n",ylab="",xlab="",xaxt=ifelse(rowNum!=(respCol-1),"n","s"))
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
           "grey89",border="grey89")
    hst<-hist(dat[,colNum],col=origCols[1],add=TRUE)
    hist(dat[dat$brushResp!="nonbrush.0",colNum],breaks=hst$breaks,add=TRUE,col=origCols[2])
    hist(dat[!dat$brushResp %in% c("nonbrush.1","nonbrush.0"),colNum],
         breaks=hst$breaks,add=TRUE,col=origCols[3])
    hist(dat[!dat$brushResp %in% c("brush.0","nonbrush.0","nonbrush.1"),colNum],
         breaks=hst$breaks,add=TRUE,col=origCols[4])
    return()
  }  
  #================================  
  #pairs plot below the diagonal

  if(rowNum>colNum){
    
    plot(x=dat[,colNum],y=dat[,rowNum],type="n",
         xaxt=ifelse(rowNum!=(respCol-1),"n","s"),
         ylab=ifelse(rowNum!=(respCol-1),"",names(dat)[rowNum]),
         yaxt=ifelse(colNum!=1,"n","s"),xlab=ifelse(colNum!=1,"",names(dat)[colNum]),tck=.01,mgp=c(3,.01,0))
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
           "grey89",border="grey89")
    points(x=dat[,colNum],y=dat[,rowNum],col=Cols[dat$brushResp],pch=16,
           cex=c(pointSize,1.6*pointSize)[factor(brushRegion)],bty="n")
            return()
    }  
  #=============================
  #color above the diagonal
  if(rowNum<colNum){ 
   
    Cols<-c("grey93",brewer.pal(9,"Reds"))
        Cor<-cor(dat[,rowNum],dat[,colNum])
        ColIndx<-cut(abs(Cor),breaks=c(0,seq(from=.6,to=1,length=length(Cols))))
        colPlot<-ggplot(d,  aes(x = x, y = y))+ geom_blank()+
          theme(panel.background = element_rect(fill = Cols[ColIndx]),
                panel.grid.major = element_line(colour = Cols[ColIndx]),
                panel.grid.minor = element_line(colour = Cols[ColIndx]),
                plot.margin=unit(c(0,1,2,0),"mm"),
                axis.ticks.length = unit(0,"null"))+
          
          ylab("")+xlab("")+scale_x_continuous(breaks=NULL,expand = c(0,0),limits=c(.2,.8))+
          scale_y_continuous(breaks=NULL,expand=c(0,0))+theme(axis.title=element_text(size=rel(1.3)))+
          annotate("text", label= round(Cor,digits=2), x=.5, y=.5,
                   size=15*abs(Cor))
        if(rowNum==1) colPlot<-colPlot+ggtitle(names(dat)[colNum])+
                theme(plot.title = element_text(face="bold"))
        return(colPlot)
  }  
}

