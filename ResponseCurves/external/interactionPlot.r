response.curvesInteraction<-function(fitLst,model,vals=NULL,theta=30,phi=25){
    
        #How to check that models and data match
        dat<-fitLst$dat$ma$train$dat[,-1]
        resp<-fitLst$dat$ma$train$dat[,1]
        VarNames<-names(dat)
        myPredict <- function (x, y, ...) { 
          out <- predict(x, y, type='response', args=c("outputformat=logistic"), ...);
          return (out)
        }
        #response(m_me,  at=mean, expand=0, fun=myPredict)
        #response(m_glm, at=mean, expand=0, fun=myPredict)
        
         mins  <- sapply(dat, min,  na.rm=TRUE)
         maxs  <- sapply(dat, max,  na.rm=TRUE)
         means <- sapply(dat, mean, na.rm=TRUE)
         n <- 50
         
        if(is.null(vals)) vals<-matrix(means,nrow=1)
         else vals<-rbind(means,vals)
         
        v=1
        x="bio1"
        y="asage_1km"
        # Cols<-c("red","blue","green","blueviolet","darkgoldenrod1","aquamarine","violetred","slateblue")
           
          
            y.lim<-c(0,1)
              nRow<-1
                          test <- do.call("rbind", replicate(n^2, vals[v,], simplify=FALSE))
                          yCol= match(y,names(dat))
                          xCol=match(x,names(dat))
                          test[, yCol] <- rep(seq(mins[yCol], maxs[yCol], length.out=n),each=n)
                          test[, xCol] <- rep(seq(mins[yCol], maxs[yCol], length.out=n),times=n)
                          test<-as.data.frame(test)
                          colnames(test)<-names(means)
                           Response<-pred.fct(fitLst$mods$final.mod, test,model)
                           z<-matrix(Response,ncol=n)
                           nrz <- nrow(z)
                           ncz <- ncol(z)
                            zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
                            nbcol<-length(Colors)
                            # Recode facet z-values into color indices
                            facetcol <- cut(zfacet, nbcol)
                          
                           persp(x=seq(mins[xCol], maxs[xCol], length.out=n),y=seq(mins[yCol], maxs[yCol], length.out=n),
                               z=z,theta=theta,phi=phi,col=Colors[facetcol],shade=.4,xlab=x,ylab=y,main=model,zlim=c(0,1),border=NA)
                           }

                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
      