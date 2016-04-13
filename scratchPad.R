brushRegion<-rep(FALSE,times=nrow(dat))
#brushRegion[c(1:116)]<-TRUE
brushRegion[c(117:length(brushRegion))]<-TRUE
ggpairs(dat,alph,pointSize,DevScore,showResp,brushRegion=brushRegion,rowNum=2,colNum=0)
ggpairs(dat,alph,pointSize,DevScore,showResp,brushRegion=brushRegion,rowNum=2,colNum=2)

respPlt<-ggplot(dat, aes(x = bio1, 
                           y =pb)) + 
  geom_point(aes(x=bio1,y=pb,colour=brushResp),alpha=alph) +
  stat_smooth(method="glm", method.args=list(family="binomial"), formula = y ~ ns(x, 3))+
  scale_fill_manual(values = Cols)+theme(legend.position="none")+
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),plot.margin=unit(c(0,1,1,0),"mm"))+
  xlab("")+ylab("")
nplts
