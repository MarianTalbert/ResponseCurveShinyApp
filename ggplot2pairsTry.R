
dat<-myBiomodData@data.env.var
dat$resp<-as.factor(myBiomodData@data.species)
par(mfrow=c(ncol(dat),ncol(dat)),mar=c(0,0,0,0),oma=c(0,0,0,0))
hist(dat[,1],main=names(dat)[1])
d<-data.frame(x=c(0,1),y=c(0,1))
grid.newpage()
pushViewport(viewport(layout=grid.layout((ncol(dat)-1),(ncol(dat)-1))))
vplayout<- function(x,y)
  viewport(layout.pos.row=x, layout.pos.col=y)
for(i in 1:(ncol(dat)-1)){
print(qplot(dat[,i],
            geom="histogram",
            xlab = names(dat)[i], 
            ylab = "",  
            fill=I("blue"),
            colour=I("black"))+scale_y_discrete(breaks=NULL)+scale_x_discrete(breaks=NULL)+
            theme(plot.margin=unit(c(0,0,0,0),"mm")),vp=vplayout(i,i))
#pairs plot below the diagonal
  if(i>1){
  for(j in 1:(i-1)){ 
  
  g<-ggplot(dat, 
         aes_q(x = as.name(names(dat)[j]), 
               y = as.name(names(dat)[i]),
               colour=as.name(names(dat)[ncol(dat)])))+ geom_point(size=1, alpha = 0.2)+
               scale_color_manual(values=c("blue","red"))+ theme(legend.position = 'none')+
               theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank(),
               plot.margin=unit(c(0,0,0,0),"mm"))+theme(axis.text.y = element_text(angle = 90, hjust = 1))
  if(j!=1) g<-g+ scale_y_discrete(breaks=NULL)+ylab("")
  if(i!=(ncol(dat)-1)) g<-g+ scale_x_discrete(breaks=NULL)+xlab("")
  print(g,vp=vplayout(i,j))             
  }  
}
#color above the diagonal
  Cols<-c("white",brewer.pal(9,"YlOrRd"))
  
  if(i<(ncol(dat)-1)){
    for(j in (i+1):(ncol(dat)-1)){
      Cor<-cor(dat[,i],dat[,j])
      ColIndx<-cut(abs(Cor),breaks=c(0,seq(from=.5,to=1,by=.1)))
      print(
        ggplot(d,  aes(x = x, y = y))+ geom_blank(size=4, alpha = 0.5)+
                     theme(panel.background = element_rect(fill = Cols[ColIndx]))+
                     theme(panel.grid.major = element_line(colour = Cols[ColIndx]))+
                     theme(panel.grid.minor = element_line(colour = Cols[ColIndx]),
                     plot.margin=unit(c(0,0,0,0),"mm"))+
                     ylab("")+xlab("")+scale_x_discrete(breaks=NULL)+
                     scale_y_discrete(breaks=NULL)+
                     annotate("text", label= round(Cor,digits=2), x=1.2, y=1.2,
                              size=15*abs(Cor)),
          vp=vplayout(i,j))
    }  
  }
    
  }
#========================================
#========================================
