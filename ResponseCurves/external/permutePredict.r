permutePredict<-function(dat,modelFit,resp){
  AUC<-matrix(NA,nrow=ncol(dat),ncol=5)
  varIncluded<-vector(length=ncol(dat))
  for(j in 1:5){ #do the permutation 5 times to remove some of the random chance component
    for (i in 1:ncol(dat)){
      Dat<-dat
      Dat[,i]<-Dat[sample(1:dim(dat)[1]),i]
      options(warn=-1)
      new.pred<-as.vector(predictBinary(modelFit,Dat))
      #have to use ROC here because auc in presence absence incorrectly assumes auc will be greater than .5
      AUC[i,j]<-roc(resp,new.pred)
      options(warn=0)
    } }
  #assuming here if the AUC doesn't change as the variable is permuted it wasn't in the model
  
  varIncluded<-(!apply(AUC,1,var)==0)
  AUC<-apply(AUC,1,mean)
  return(list(AUC=AUC,varIncluded=varIncluded))
}