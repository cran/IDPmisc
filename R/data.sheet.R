"data.sheet" <-
function(x){
  ## Coerces a list with vectors of different length into a data.frame
  ## fills the shorter vectors with NA
  ##
  ## Author: Thomas Unternaehrer
  ## Version 18.1.05
  if (!is.list(x)) stop(paste(deparse(x),"must be of type list"))
  return(sapply(x, function(y) {length(y) <- max(sapply(x,length));y}))
}

