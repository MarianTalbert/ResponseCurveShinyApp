GetParkBoundary<-function(dat,Bndry,ParkCode,UNIT_CODE=NA,Buffer=NA){
    #this gets the park boundary creates a buffer if desired and
    #returns the boundary transformed to lat lon
    #   Bndry = the location of the shapefile
    #   Buffer = distance in meteres
    #   ParkCode = 4 letter character string specifying the park of interest
 
  ProjInfo<-readOGR(dirname(Bndry),gsub(".shp","",basename(Bndry)))
    ProjInfo<-proj4string(ProjInfo)
    

  if(!is.na(UNIT_CODE)){
        indx<-match(UNIT_CODE,names(dat))
        dat<-dat[which(toupper(dat[[indx]])==toupper(ParkCode),arr.ind=TRUE),]
    }
   
   proj4string(dat)<-ProjInfo
  if(!is.na(Buffer)){  
    Buffer<-as.numeric(substr(Buffer,start=1,stop=(nchar(Buffer)-2)))*1000
    #we have to transform to "blah" to lcc so we have same dist x and y
    dat<-spTransform(dat, 
      CRS("+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))  
    #B<-spTransform(Bounds, CRS("+proj=utm")) 
    dat<-gBuffer(dat,width=Buffer)
  }
 
Bounds<-spTransform(dat, CRS("+proj=longlat")) 
return(Bounds)
}
