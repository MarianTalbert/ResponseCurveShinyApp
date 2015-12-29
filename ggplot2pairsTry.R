
dat<-myBiomodData@data.env.var
dat$resp<-as.factor(myBiomodData@data.species)
par(mfrow=c(ncol(dat),ncol(dat)),mar=c(0,0,0,0),oma=c(0,0,0,0))
hist(dat[,1],main=names(dat)[1])

grid.newpage()
pushViewport(viewport(layout=grid.layout(ncol(dat),ncol(dat))))
vplayout<- function(x,y)
  viewport(layout.pos.row=x, layout.pos.col=y)
for(i in 1:(ncol(dat)-1)){
print(qplot(dat[,i],
            geom="histogram",
            main = names(dat[,i]), 
            xlab = "Age",  
            fill=I("blue")),vp=vplayout(i,i))
#pairs plot below the diagonal
  if(i>1){
  for(j in 1:(i-1)){
  print(
  ggplot(dat, 
         aes_q(x = as.name(names(dat)[1]), 
               y = as.name(names(dat)[2]),
               colour=as.name(names(dat)[ncol(dat)])))+ geom_point(size=4, alpha = 0.5)+
               scale_color_manual(values=c("blue","red"))+ theme(legend.position = 'none'),vp=vplayout(i,j))
  }  
}
#color above the diagonal
  if(i<(ncol(dat)-1)){
    for(j in 1:(i-1)){
      print(
        ggplot(dat, 
               aes_q(x = as.name(names(dat)[1]), 
                     y = as.name(names(dat)[2]),
                     colour=as.name(names(dat)[ncol(dat)])))+ geom_blank(size=4, alpha = 0.5)+
                     theme(panel.background = element_rect(fill = "red"))+
          scale_color_manual(values=c("blue","red"))+ theme(legend.position = 'none'),vp=vplayout(i,j))
    }  
  }
    
  }
#========================================
#========================================
changeMonth <- base::as.Date("1996-02-15") #as.Date("1995-04-19") + lubridate::weeks(39) = "1996-01-17"

vpLayout <- function(x, y) { grid::viewport(layout.pos.row=x, layout.pos.col=y) }

fullSpread <- function( scores ) { 
  return( base::range(scores) ) #A new function isn't necessary.  It's defined in order to be consistent.
}
hSpread <- function( scores ) { 
  return( stats::quantile(x=scores, probs=c(.25, .75)) ) 
}
seSpread <- function( scores ) { 
  return( base::mean(scores) + base::c(-1, 1) * stats::sd(scores) / base::sqrt(base::sum(!base::is.na(scores))) ) 
}
bootSpread <- function( scores, conf=.68 ) {
  plugin <- function( d, i ) { base::mean(d[i]) }
  
  distribution <- boot::boot(data=scores, plugin, R=99) #999 for the publication
  ci <- boot::boot.ci(distribution, type = c("bca"), conf=conf)
  return( ci$bca[4:5] ) #The fourth & fifth elements correspond to the lower & upper bound.
}

darkTheme <- ggplot2::theme(
  axis.title          = ggplot2::element_text(color="gray30", size=9),
  axis.text.x         = ggplot2::element_text(color="gray30", hjust=0),
  axis.text.y         = ggplot2::element_text(color="gray30"),
  axis.ticks.length   = grid::unit(0, "cm"),
  axis.ticks.margin   = grid::unit(.00001, "cm"),
  #   panel.grid.minor.y  = element_line(color="gray95", size=.1),
  #   panel.grid.major    = element_line(color="gray90", size=.1),
  panel.margin        = grid::unit(c(0, 0, 0, 0), "cm"),
  plot.margin         = grid::unit(c(0, 0, 0, 0), "cm")
)

p <- ggplot(dat, 
              aes_q(x = as.name(names(dat)[1]), 
                    y = as.name(names(dat)[2]),
                    colour=as.name(names(dat)[ncol(dat)])))+ geom_point(size=4, alpha = 0.5)+
                   scale_color_manual(values=c("blue","red"))

print(qplot(dat[,i],
      geom="blank",
      main = names(dat[,i]), 
      xlab = "Age",  
      fill=I("blue")),vp=vplayout(1,2))

print(TmaxCompare+ ggtitle(ParkName)+theme(plot.title =element_text(size=rel(2.5))), vp=vplayout(1,1))
print(TminCompare, vp=vplayout(2,1))
print(PptCompare, vp=vplayout(3,1))
dev.off()   

print(qplot(dat[,1],
      geom="histogram",
      binwidth = 0.5,  
      main = names(dat[,1]), 
      xlab = "Age",  
      fill=I("blue")),vp=vplayout(1,1))