univariateDev<-function(dat){
  devExp<-vector()
  GamRan<-rep(TRUE,ncol(dat)-1)
  for(i in 1:(ncol(dat)-1)){
      form<-as.formula(paste(eval(names(dat)[ncol(dat)]),"~","s (",eval(names(dat[i])),",2)"))
      g<-try(gam:::gam(formula=form,data=dat,family=binomial),silent=TRUE)
      
      if(inherits(g,"try-error")){ 
        form<-as.formula(paste(eval(names(dat)[ncol(dat)]),"~",eval(names(dat[i]))))
        g<-glm(form,data=dat,family=binomial)
        GamRan[i]<-FALSE
      }
      devExp[i]<-signif(100*(1-g$deviance/g$null.deviance),digits=3) 
  }
  return(list(devExp=devExp,GamRan=GamRan))
}   
