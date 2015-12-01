setMethod("predict",signature("BIOMOD.models.out"),
          definition=function(object,Data,selected.models){
            #since biomod2 models don't have a predict method I'll just write my own
            pro<-BIOMOD_Projection(object,Data,proj.name="new",selected.models=selected.models,
                                   keep.in.memory=TRUE,on_0_1000=FALSE)
            return(as.vector(pro@proj@val))
 })

