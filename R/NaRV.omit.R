"NaRV.omit" <-
function(x){
  ## Author: Rene Locher
  ## Version: 2005-04-20
  
  if (is.vector(x)) {
    if (is.numeric(x)) x <- na.omit(x[is.finite(x)]) else 
    x <- na.omit(x)
  } else if (is.factor(x)) {
    x <- na.omit(x)
  } else if (is.data.frame(x)) {
    x.num <- as.matrix(x[,sapply(x, is.numeric)])
    ri <- (!apply(x.num,MAR=1,function(x) sum(is.infinite(x))>0) &
    !apply(x,MAR=1,function(x) sum(is.na(x))>0))
    x <- x[ri,,drop=FALSE]
    ## class omit is incompatible with class data.frame
    ## attributes(x) <- c(attributes(x),list(na.action=which(!ri),class="omit"))
  } else if (is.matrix(x)) {
    if(is.numeric(x)) {
      ri <- !apply(x,MAR=1,function(x) {sum(is.na(x)|is.infinite(x))>0})
      x <- x[ri,,drop=FALSE]
      attributes(x) <- c(attributes(x),list(na.action=which(!ri),class="omit"))
    } else x <- na.omit(x)
  } else {
    warning("\n'x' is neither a vector, nor a factor, nor a data.frame nor a matrix. \n'x' is returned unchanged")
    return(x)
  }
  return(x)
}## NaRV.omit

