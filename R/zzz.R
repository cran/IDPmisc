setClass("rose",
         slots = c(rho="matrix",
                        cyclVar="numeric",
                        circle="numeric"))

setValidity("rose", val.rose)

setMethod("plot", signature(x = "rose",y = "missing"), plot.rose)

.onLoad <- function(libname, pkgname){
  library.dynam("IDPmisc", package=pkgname, lib.loc=libname)
}

.onUnLoad <- function(libpath){
  library.dynam.unload("IDPmisc",libpath=libpath)
}
