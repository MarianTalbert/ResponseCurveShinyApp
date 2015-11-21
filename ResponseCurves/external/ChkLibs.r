ChkLibs <- function(libs){
#Checks libraries and installs any that are missing
#documentation on all libraries required by SAHM 
#Written by Marian Talbert 2/2012
      lib.mssg <- unlist(suppressMessages(suppressWarnings(lapply(libs,require,quietly = T, warn.conflicts=F,character.only=T))))
      if(any(!lib.mssg)){
            install.packages(unlist(libs[!lib.mssg]), repos = "http://cran.r-project.org")
            lib.mssg <- unlist(suppressMessages(suppressWarnings(lapply(libs,require,quietly = T, warn.conflicts=F,character.only=T))))
            }
        if(any(!lib.mssg)) stop(paste(paste("\n\nthe following package(s) could not be loaded: ",paste(unlist(libs[!lib.mssg]),sep="")),sep=""))

}


