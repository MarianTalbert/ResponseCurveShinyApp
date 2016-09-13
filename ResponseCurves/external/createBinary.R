#if (!isGeneric("createBinary")) {
setGeneric("createBinary", function(Predicted,thresh, ...)
standardGeneric("createBinary"))
#}


setMethod('createBinary', signature(Predicted='RasterLayer'),
function(Predicted,thresh, filename='', ...) {
#based on recommendation from the raster package vignette
  out <- raster(Predicted)
   big <- ! canProcessInMemory(out, 3)
   filename <- trim(filename)
   if (big & filename == '') {
   filename <- rasterTmpFile()
   }
   if (filename != '') {
   out <- writeStart(out, filename, ...)
   todisk <- TRUE
  } else {
   vv <- matrix(ncol=nrow(out), nrow=ncol(out))
   todisk <- FALSE
   }
  
   bs <- blockSize(Predicted)
   pb <- pbCreate(bs$n, ...)
  
   if (todisk) {
   for (i in 1:bs$n) {
   v <- getValues(Predicted, row=bs$row[i], nrows=bs$nrows[i] )
 
   v <- as.numeric(v > thresh)
   out <- writeValues(out, v, bs$row[i])
   pbStep(pb, i)
   }
   out <- writeStop(out)
   } else {
   for (i in 1:bs$n) {
   v <- getValues(Predicted, row=bs$row[i], nrows=bs$nrows[i] )
   v <- as.numeric(v > thresh)
   cols <- bs$row[i]:(bs$row[i]+bs$nrows[i]-1)
   vv[,cols] <- matrix(v, nrow=out@ncols)
   pbStep(pb, i)
   }
   out <- setValues(out, as.vector(vv))
   }
   pbClose(pb)
  return(out)
}
)

setMethod('createBinary', signature(Predicted='vector'),
function(Predicted,thresh, ...) {
 v <- as.numeric(Predicted > thresh)
 }
)