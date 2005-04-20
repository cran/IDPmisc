"Image" <-
function(x, y=NULL, pixs=1, zmax=NULL, colramp=IDPcolorRamp,
                  factors=c(FALSE,FALSE))
  ## Author: Andreas Ruckstuhl, refined by Rene Locher
  ## Version: 2005-04-19
{
  if (is.matrix(x) | is.data.frame(x)) {
    if (ncol(x)>1) {
      if (is.null(y)) y <- x[,2] else
          stop("'x' must have only 1 column when y is supplied separately")
    }
    x <- x[,1]
  }
  
  if (is.null(y)) {
    y <- x
    x <- 1:length(x)
  }
  if (length(y)!=length(x)) stop("Vector 'y' must have the same length as 'x'")
  
  xy <- NaRV.omit(data.frame(x,y))
  factors <- factors | sapply(xy,is.factor)
  xy <- sapply(xy,as.numeric)

  pixs <- (pixs/10)/2.54
  usr <- par("usr")
  
  if (factors[1]) {
    bx <- seq(min(xy[,1]-0.25), max(xy[,1]+0.25),
              length=2*diff(range(xy[,1]))+2)
  } else {
    bx <- seq(usr[1],usr[2], length=round(par("pin")/pixs)[1]+1)
  }

   if (factors[2]) {
     by <- seq(min(xy[,2]-0.25), max(xy[,2]+0.25),
               length=2*diff(range(xy[,2]))+2)
   } else {
     by <- seq(usr[3],usr[4], length=round(par("pin")/pixs)[2]+1)
   }
  
  zz <- table(cut(xy[,1],b=bx), cut(xy[,2], b=by))
  zzmax <- max(zz)

  if(is.null(zmax)) zmax <- zzmax
  if(zmax<1||is.null(zmax)) {
    stop("\nzmax must be >= 1 and
          plot(x,y,...) must have been called before calling this function!")
  }
  
  if(zzmax>zmax)
    stop("\nzmax too small! Densiest aereas are out of range!",call. = FALSE)

  zmax <- max(zmax,2) ## zmax must not be smaller than 2!

  ## bg color for 0
  lbx <- length(bx)
  lby <- length(by)

  image(x=0.5*(bx[-1]+bx[-lbx]), y=0.5*(by[-1]+by[-lby]), zz,
        col=colramp(zmax), breaks=seq(0.5,zmax+1,1),
        xaxs="r", yaxs="r", add=TRUE)
  box()
  invisible(zzmax)
} # Image

