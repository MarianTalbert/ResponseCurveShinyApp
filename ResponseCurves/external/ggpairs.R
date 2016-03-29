ggpairs<-function(dat,alph,pointSize,DevScore,showResp,brushRegion,rowNum,colNum){

  d<-data.frame(x=c(0,1),y=c(0,1))
  colOffset<-ifelse(showResp,1,0) 
  dat[,ncol(dat)]<-as.numeric(as.character(dat[,ncol(dat)])) 
  
  #===================
  #response column
  if(colNum==0){
    dat[,ncol(dat)]<-as.numeric(dat[,ncol(dat)]) 
      respPlt<-ggplot(dat, aes_q(x = as.name(names(dat)[rowNum]), 
                                 y =as.name(names(dat)[ncol(dat)]),
                                 colour=as.name(names(dat)[ncol(dat)]))) + 
        geom_point(alpha=alph) +
        stat_smooth(method="glm", method.args=list(family="binomial"), formula = y ~ ns(x, 3))+
        scale_color_gradient(low="blue",high="red")+theme(legend.position="none")+
        theme(panel.grid.minor=element_blank(),
              panel.grid.major=element_blank(),plot.margin=unit(c(0,1,1,0),"mm"))+
        xlab("")+ylab("")
      
      if(rowNum==1) respPlt<-respPlt+ggtitle("Response")
      return(respPlt)
    }
  
  dat[,ncol(dat)]<-as.factor(dat[,ncol(dat)]) 
  
  #================================
  #histogram above the diagonal
  if(rowNum==colNum){ 
  
      hst<-ggplot(dat,
                  aes_q(x=as.name(names(dat[rowNum]))))+
                  geom_histogram(fill="blue")+theme(plot.margin=unit(c(0,1,1,0),"mm"))+
       xlab("")
      if(colNum!=1) hst<-hst+ylab("")
      if(rowNum==1) hst<-hst+ylab(paste(names(dat[rowNum]),"\n",sep=""))+ggtitle(names(dat[rowNum]))+ 
                    theme(axis.title=element_text(size=rel(1.3)))
      if(rowNum==(ncol(dat)-1)) hst<-hst+xlab("\n")
      return(hst)
      #scale_y_discrete(breaks=NULL)+scale_x_continuous(breaks=NULL)+
  }  
  #================================  
  #pairs plot below the diagonal
  if(rowNum>colNum){
    newDat<-data.frame(x=dat[,colNum],y=dat[,rowNum],col=dat[,ncol(dat)])
       
    g<-ggplot(newDat, 
               aes(x = x, 
                     y = y,
                     colour=col))+ geom_point(size=pointSize, alpha = alph)+
                     scale_color_manual(values=c("blue","red"))+ theme(legend.position = 'none')+
                     theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank(),
                     plot.margin=unit(c(0,0,0,0),"mm"))+ 
                     theme(axis.text.y = element_text(angle = 90))+
                     theme(axis.text = element_text(size = rel(1.3)),axis.title=element_text(size=rel(1.3)))  
        if(colNum!=1) g<-g+ylab("") +#scale_y_discrete(breaks=NULL)
        if(rowNum!=1) g<-g+xlab("")
        if(rowNum!=(ncol(dat)-1)) g<-g #+scale_x_discrete(breaks=NULL)
        return(g)         
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

