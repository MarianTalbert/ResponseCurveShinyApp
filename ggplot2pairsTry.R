#Using the biomod data
dat<-myBiomodData@data.env.var
dat$resp<-as.factor(myBiomodData@data.species)

#using the dismo data
dat<-sdmdata[,c(4:7,3)]
dat[,ncol(dat)]<-as.factor(dat[,ncol(dat)])

d<-data.frame(x=c(0,1),y=c(0,1))
pointSize<-ifelse(nrow(dat)>1000,1,3)

grid.newpage()
pushViewport(viewport(layout=grid.layout((ncol(dat)-1),(ncol(dat)))))
vplayout<- function(x,y)
  viewport(layout.pos.row=x, layout.pos.col=y)

dat[,ncol(dat)]<-as.numeric(as.character(dat[,ncol(dat)])) 
for(j in 1:(ncol(dat)-1)){
  print(  
    ggplot(dat, aes_q(x = as.name(names(dat)[j]), 
                      y =as.name(names(dat)[ncol(dat)]),
                      colour=as.name(names(dat)[ncol(dat)]))) + 
      geom_point(alpha=.2) +
      stat_smooth(method="glm", family="binomial", formula = y ~ ns(x, 2))+
      scale_color_gradient(low="blue",high="red")+theme(legend.position="none")+
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),plot.margin=unit(c(0,0,0,0),"mm"))+
      xlab(names(dat)[j])+ylab("response")+scale_y_continuous(breaks=NULL), 
    vp=vplayout(j,1))
}

dat[,ncol(dat)]<-as.factor(dat[,ncol(dat)]) 
for(i in 1:(ncol(dat)-1)){

print(qplot(dat[,i],
            geom="histogram",
            xlab = names(dat)[i], 
            ylab = "",  
            fill=I("blue"))+scale_y_discrete(breaks=NULL)+scale_x_discrete(breaks=NULL)+
            theme(plot.margin=unit(c(0,0,0,0),"mm")),
      vp=vplayout(i,i+1))
#pairs plot below the diagonal
  if(i>1){
  for(j in 1:(i-1)){ 
  
  g<-ggplot(dat, 
         aes_q(x = as.name(names(dat)[j]), 
               y = as.name(names(dat)[i]),
               colour=as.name(names(dat)[ncol(dat)])))+ geom_point(size=pointSize, alpha = .2)+
               scale_color_manual(values=c("blue","red"))+ theme(legend.position = 'none')+
               theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank(),
               plot.margin=unit(c(0,0,0,0),"mm"))+theme(axis.text.y = element_text(angle = 90, hjust = 1))+
               scale_y_discrete(breaks=NULL)
  if(j!=1) g<-g+ylab("")
  if(i!=(ncol(dat)-1)) g<-g+ scale_x_discrete(breaks=NULL)+xlab("")
  print(g,vp=vplayout(i,j+1))             
  }  
}
#color above the diagonal
  Cols<-c("white",brewer.pal(9,"Reds"))
  
  if(i<(ncol(dat)-1)){
    for(j in (i+1):(ncol(dat)-1)){
      Cor<-cor(dat[,i],dat[,j])
      ColIndx<-cut(abs(Cor),breaks=c(0,seq(from=.6,to=1,length=length(Cols))))
      print(
        ggplot(d,  aes(x = x, y = y))+ geom_blank(size=4, alpha = 0.5)+
                     theme(panel.background = element_rect(fill = Cols[ColIndx]))+
                     theme(panel.grid.major = element_line(colour = Cols[ColIndx]))+
                     theme(panel.grid.minor = element_line(colour = Cols[ColIndx]),
                     plot.margin=unit(c(0,0,0,0),"mm"))+
                     ylab("")+xlab("")+scale_x_continuous(breaks=NULL)+
                     scale_y_continuous(breaks=NULL)+
                     annotate("text", label= round(Cor,digits=2), x=.5, y=.5,
                              size=15*abs(Cor)),
          vp=vplayout(i,j+1))
    }  
  }
    
}


#========================================
#========================================
