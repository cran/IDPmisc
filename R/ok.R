"ok" <-
function(x) {
  ## Author: Rene Locher
  ## Version: 2005-03-01
  if (is.logical(x)) {
    x[is.na(x)] <- FALSE
    return(x)
  } else
  stop("'x' must be logical!")
} ## ok 

