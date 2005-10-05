"rose" <-
function(x, cyclVar=NULL, circle=NULL, n.cyclVar=8, 
                 cut=NULL, breaks=NULL, labels=NULL,
                 dig.lab=2, include.lowest = FALSE,
                 subset=NULL, na.warning=TRUE, FUN=mean, ...)
  ## Author: Rene Locher
  ## Version: 2005-09-30
  ##
  ## Calculates aggregates as function of cyclic variable like direction or hour
  ##  of day and, possibly, a second variable for splitting the data.
  ## Examples: The mean value of ozone concentration as a function of wind 
  ##           direction and windspeed.
  ##           The mean value of ozone, nitrogen dioxide, nitrogen oxid, etc. as
  ##           a function of hour of day.
  ##           The quantiles of ozone concentration as a function of wind
  ##           direction
  ##
  ## Details:
  ## Only selected combinations of the variables x, cyclVar, cut and
  ## FUN output are allowed.
  ## allowed combinations:
  ## x with ncol(x) >= 1: cyclVar, FUN output of length 1
  ## x with ncol(x) =  1: cyclVar, FUN output of length 1 and more
  ## x with ncol(x) =  1: cyclVar, FUN output of length 1, cut
  ##
  ##
  ## Output:
  ## The result is stored in a 2-dimensional matrix 'rho' with the rows
  ## corresponding to the values of the bin centers of the cyclic variable.
  ##
  ## The different columns correspond to
  ## the splitting levels of the second variable or to
  ## the different x column when x has more than one column or to
  ## the different elements of the output vector of FUN.
  ##
  ## The centers of the cyclic variable are stored in the vector 'cyclVar'
  ##
  
{
  lab <- deparse(substitute(x))

  if (is.vector(x)) {
    x <- as.matrix(x)
    colnames(x) <- lab 
  } else  if (!(is.matrix(x))) x <- as.matrix(x) else
  stop("'x' must be a matrix, a data.frame or a vector!")

  if(is.null(circle)) stop("'circle' is not defined!")

  if (is.null(cyclVar)) stop("'cyclVar' is not defined!") else
  if (!is.numeric(cyclVar)) stop("'cyclVar' must be numeric!") else
  if (is.factor(cyclVar)) stop("'cyclVar' must not be a factor!") else
  if (sum(cyclVar<0,na.rm=TRUE)+sum(cyclVar>=circle,na.rm=TRUE)>0)
    stop("The following condition must be full filled: \n0 <= 'cyclVar' < 'circle'")
  
  if (nrow(x) != length(cyclVar))
    stop("\nx and cyclVar have incompatible dimensions!")      
  if (is.null(colnames(x)))
    colnames(x) <- paste("x",1:ncol(x),sep="")

  if (!is.null(subset)) {
    subset[is.na(subset)] <- F
    cyclVar <- cyclVar[subset]
    if (!is.null(cut)) cut <- cut[subset]
    x <- x[subset,,drop=FALSE]
  }
  
  ii <- is.na(cyclVar)
  if (sum(ii)&na.warning){
    warning("cyclVar' must not contain NAs! Corresponding data are omitted.\n\n")
    x <- x[!ii,,drop=FALSE]
    cyclVar <- cyclVar[!ii]
    if (!is.null(cut)) cut <- cut[!ii]
  }

  ## make sure that label are numerics
  cyclVar.lab <- circle/n.cyclVar*(0:(n.cyclVar-1))

  ## labels are the center of the category
  cyclVar.f <- cut((cyclVar+circle/n.cyclVar/2)%%circle,
               breaks=c(cyclVar.lab,circle),
               labels=cyclVar.lab,include.lowest=T)

  if (ncol(x)>1) {
    if (!is.null(cut)|!is.null(breaks))
      warning("When x has more than 1 column, 'cut' and 'breaks' are not used!")
    res <- aggregate(x, by=list(cyclVar=cyclVar.f), FUN=FUN, ...)
    res$cyclVar <- as.numeric(as.character(res$cyclVar))

    ## make sure that all possible values are included!
    res <- merge(cyclVar.lab, res, by.x=1, by.y="cyclVar", all=TRUE)
    res <- as.matrix(res[,-1])
  } else if (is.null(cut)) {
    ## statistics with possibly more than a single value per level of cyclVar.f
    res <- tapply(unlist(x), INDEX=list(cyclVar=cyclVar.f), FUN=FUN, ...)
    len <- sapply(res,length)
    if (length(len)>1) {
      if (diff(range(len))!=0) {
        ii <- len == 0
        if (sum(ii)>0) res <- res[!ii]
        if (diff(range(sapply(res,length)))!=0) {
          print(res)
          stop("\n'FUN' must return a vector of fixed length!")
        } 
      }
    }
    
    cnam <- names(res[[1]])
    cyclVar <- as.numeric(names(res))
    res <- cbind(cyclVar,t(matrix(unlist(res),ncol=length(res))))

    ## make sure that all possible values are included!
    res <- merge(cyclVar.lab, res, by.x=1, by.y=1, all=TRUE)
    res <- as.matrix(res[,-1])

    if (!is.null(cnam)) colnames(res) <- cnam
    else colnames(res) <- paste("V",1:ncol(res),sep="")
  } else { ## 2 D statistics with one column
    if (nrow(x)!=length(cut)) stop("The vector 'cut' must have the same length as vector'x'")
    if (is.factor(cut)) {
      cut.f <- cut} else
    if (is.logical(cut)) {
      cut.f <- factor(cut)} else {
      if (is.null(breaks)) {
        breaks <- pretty(c(cut,max(cut)+diff(range(cut)*0.01)),n=3)
      }
      cut.f <- cut(cut, breaks=breaks, labels=labels, dig.lab=dig.lab,
                   include.lowest=include.lowest)
    }

    if (length(eval(FUN(x[,1])))>1)
      stop("\When 'cut' is not NULL, 'FUN' must return a scalar!")
    agg <- aggregate(x, by=list(cyclVar=cyclVar.f,cut=cut.f), FUN=FUN, ...)

    ## Make sure that all combination are in the matrix
    res <- expand.grid(cyclVar=cyclVar.lab,cut=levels(cut.f))
    res <- merge(res,agg,all.x=TRUE)

    res <- res[order(as.numeric(res$cut),as.numeric(res$cyclVar)),]

    res <- matrix(res[,3],nrow=n.cyclVar, byrow=FALSE)

    colnames(res) <- if (is.null(labels)) levels(cut.f) else labels
  }
  
  rownames(res) <- cyclVar.lab
  return(new("rose",rho=res,cyclVar=cyclVar.lab,circle=circle))
} ## rose

