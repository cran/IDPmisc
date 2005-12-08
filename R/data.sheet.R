"data.sheet" <-
function(x){
  ## Coerces a list with vectors of different length into a data.frame
  ## fills the shorter vectors with NA
  ##
  ## Authors: Thomas Unternaehrer, René Locher
  ## Version 29.09.05
  if (!is.list(x)) stop("'x' must be of type list\n")
  if (min(sapply(x,length)) == 0) stop("NULL elements in list not allowed\n")
  return(sapply(x, function(y) {length(y) <- max(sapply(x,length));y}))
}

