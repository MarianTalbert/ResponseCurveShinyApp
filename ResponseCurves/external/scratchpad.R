ggplot(data = distribs, aes(x = values, group = type)) +
  geom_density(aes(fill = type, color = type), alpha = 0.5)

m <- ggplot(movies, aes(x = rating))
m + geom_density()
d<-data.frame(predicted=unlist(predictedVals),Response=rep(resp,times=length(fitLst)),
                Model=rep(names(fitLst),each=length(resp)))
d$Model<-as.factor(d$Model)
d$Response<-as.factor(d$Response)
m<-ggplot(d,aes(x=predicted,colour=Response,fill=Response))+geom_density(alpha=0.3)+facet_wrap(~ Model)+
  scale_fill_manual(values=c("blue","red"))+
  scale_colour_manual(values=c("blue","red"))
m

ggplot(data = distribs, aes(x = values, group = type)) +
  geom_density(aes(fill = type, color = type), alpha = 0.5)

ggplot(Dat,aes(x=Temp,colour=Emissions,fill=Emissions)) +
  scale_fill_manual(values=EmissionsCol)+
  scale_colour_manual(values=EmissionsCol)+
  geom_density(alpha=0.3) +
  theme_bw() +
  theme0(plot.margin = unit(c(1,0,0,2.2),"lines"))