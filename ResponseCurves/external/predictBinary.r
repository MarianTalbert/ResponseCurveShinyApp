predictBinary<-function(model,x){
#Written by Marian Talbert to predict a binary response
#as some predict methods need extra args that I can't handle with just 
#standard predict methods
# x     = a data frame without the response column obviously
# Model = one of mars, glm, rf, brt, maxlike at present 

 

  if(inherits(model,"randomForest")){
                      y <-predict(model,type="response")
                      y[y>=1] <- .99999999999999999
                      y[y<=0] <- .00000000000000001
    return(y)
  }
   if(inherits(model,"gbm")){
       y <- predict(model,type="response",n.trees=model$n.trees)
       return(y)
   }
  #default to standard predict
  y<-predict(model,x,type='response')
return(y)
}