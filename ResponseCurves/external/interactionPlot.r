interactionPlot<-function(fitLst,model,vals=NULL,theta=30,phi=25,x,y,dat,resp,modelIndx=NA){
    
        VarNames<-names(dat)
        biomd=inherits(fitLst,"BIOMOD.models.out") 
        if(!biomd) fitLst=fitLst[[modelIndx]]
        Col=rev(inferno(256,begin=.1,end=1))

         mins  <- sapply(dat, min,  na.rm=TRUE)
         maxs  <- sapply(dat, max,  na.rm=TRUE)
         means <- sapply(dat, mean, na.rm=TRUE)
         vals<-as.vector(vals)
         if(is.null(vals)) vals<-means
         n <- 100
         test <- do.call("rbind", replicate(n^2, vals, simplify=FALSE))
         yCol <- match(y,names(dat))
         xCol <-match(x,names(dat))
         
         test[, yCol] <- rep(seq(mins[yCol], maxs[yCol], length.out=n),each=n)
         test[, xCol] <- rep(seq(mins[xCol], maxs[xCol], length.out=n),times=n)
         test <- as.data.frame(test)
         colnames(test) <- names(means)
         
         if(biomd) Response<-predict(fitLst, test,model)
         else Response<-predictBinary(model=fitLst, newdata=test)
         z <- matrix(Response,ncol=n)
         nrz <- nrow(z)
         ncz <- ncol(z)
         zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
         nbcol <- length(Col)
          # Recode facet z-values into color indices
         facetcol <- cut(zfacet, nbcol)
         Xlab <- paste(substr(x,start=1,stop=12),c("\n","")[1+(nchar(x)<=12)],
                 substr(x,start=13,stop=nchar(x)),sep="")
         Ylab <- paste(substr(y,start=1,stop=12),c("\n","")[1+(nchar(y)<=12)],
                 substr(y,start=13,stop=nchar(y)),sep="")       
        persp(x=seq(mins[xCol], maxs[xCol], length.out=n),y=seq(mins[yCol], maxs[yCol], length.out=n),
             z=z,theta=theta,phi=phi,col=Col[facetcol],shade=.4,xlab=Xlab,ylab=Ylab,zlab="Prediction",
             main=model,zlim=c(0,1),border=NA,cex.lab=1.3)
}

                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
      