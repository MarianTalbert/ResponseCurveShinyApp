library(biomod2)
DataSpecies <- read.csv(system.file("external/species/mammals_table.csv",
                                    package="biomod2"), row.names = 1)
head(DataSpecies)

# the name of studied species
myRespName <- 'GuloGulo'

# the presence/absences data for our species 
myResp <- as.numeric(DataSpecies[,myRespName])

# the XY coordinates of species data
myRespXY <- DataSpecies[,c("X_WGS84","Y_WGS84")]


# Environmental variables extracted from BIOCLIM (bio_3, bio_4, bio_7, bio_11 & bio_12)
myExpl = stack( system.file( "external/bioclim/current/bio3.grd", 
                             package="biomod2"),
                system.file( "external/bioclim/current/bio4.grd", 
                             package="biomod2"), 
                system.file( "external/bioclim/current/bio7.grd", 
                             package="biomod2"),  
                system.file( "external/bioclim/current/bio11.grd", 
                             package="biomod2"), 
                system.file( "external/bioclim/current/bio12.grd", 
                             package="biomod2"))

# 1. Formatting Data
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = myExpl,
                                     resp.xy = myRespXY,
                                     resp.name = myRespName)
SpDataFrame<-cbind(myBiomodData@coord,myBiomodData@data.species,myBiomodData@data.env.var)
correlationViewer(data=SpDataFrame)
# 2. Defining Models Options using default options.
myBiomodOption <- BIOMOD_ModelingOptions()

# 3. Doing Modelisation

myBiomodModelOut <- BIOMOD_Modeling( myBiomodData, 
                                     models = c('GLM','GBM','GAM','CTA','ANN',
                                                'FDA','MARS','RF'), 
                                     models.options = myBiomodOption, 
                                     NbRunEval=1, 
                                     DataSplit=70, 
                                     models.eval.meth = c('TSS','ROC'),
                                     SaveObj = TRUE,
                                     do.full.models = FALSE)

# 4. Loading some models built
ModelNames<-substr(myBiomodModelOut@models.computed,nchar(myBiomodModelOut@models.computed)-2,
                   nchar(myBiomodModelOut@models.computed))
ModelNames<-unique(gsub("_","",ModelNames))
ModelNames<-gsub("ARS","MARS",ModelNames)
myLoadedModels <- BIOMOD_LoadModels(myBiomodModelOut, models=ModelNames)

ModelNames<-myBiomodModelOut@models.computed
FitLst<-lapply(ModelNames,get_formal_model)
eval("GuloGulo_AllData_RUN1_GBM")
get_formal_model(GuloGulo_AllData_RUN1_GBM)
