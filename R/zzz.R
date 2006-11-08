.onLoad <- function(libname, pkgname){
  library.dynam("IDPmisc", package=pkgname, lib.loc=libname)
}

.onUnLoad <- function(libpath){
  library.dynam.unload("IDPmisc",libpath=libpath)
}
