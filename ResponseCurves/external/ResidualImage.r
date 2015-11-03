residImage<-function(x,y,z,boundary=NA,predictedStk,i,rastColors){
# Written by Marian Talbert 2011.
        maxSize<-1000
       if(length(z)>maxSize){
           samp<-seq(1:length(z))[order(runif(length(z)))][1:maxSize]
           z<-z[samp]
           x<-x[samp]
           y<-y[samp]
           }
      
     MinCol<-min(z)
     MaxCol<-max(z)
    
     Colors<-c(colorRampPalette(c("blue","cyan","white"))(10),"white",colorRampPalette(c("white","yellow","red"))(10))
     BreakRng<-extendrange(c(-max(abs(z)),max(abs(z))))  
     Breaks<-seq(from=BreakRng[1],to=BreakRng[2],length=length(Colors)+1)
         
     f<-function(a,b) sqrt((a-b)^2)
     
     s1<-seq(from=MinCol,to=MaxCol,length=length(length(Breaks)))
         col.ind<-apply((outer(s1,z,f)),2,which.min)
         colrange<-seq(from=MinCol,to=MaxCol,length=100)
                  points(x,y,bg=Colors[cut(z,Breaks)], pch=21,cex=abs(z)*1.5)
                   plot(raster(matrix(data=colrange, ncol=length(colrange),nrow=1)),legend.only=TRUE,col=Colors)
              }
