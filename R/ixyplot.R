"ixyplot" <-
function(x,
                    y=NULL,
                    pixs=1,
                    zmax=NULL,
                    colramp=IDPcolorRamp,
                    border=FALSE,
                    xlab=NULL,
                    ylab=NULL,
                    nx.lab=5,
                    ny.lab=5,
                    main=NULL,
                    cex.main=par("cex.main"))
  ## Produces an image scatter plot of a large 2d dataset.
  
  ## based on R function lag.plot V1.7
  ## Authors: Andreas Ruckstuhl, refined by René Locher
  ## Version 16-03-05
{
  no.xlab <- is.null(xlab)
  no.ylab <- is.null(ylab)
  no.y <- is.null(y)

  if(no.xlab) xlab <- deparse(substitute(x))
  if(no.ylab) ylab <- deparse(substitute(y))

  if(is.data.frame(x)|is.matrix(x)){
    if(ncol(x)>1) {
      if(!no.y) stop("'y' must be NULL when x is a matrix or data.frame with more than 1 column!")
      if (no.xlab) xlab <- colnames(x)[1]
      if (no.ylab) ylab <- colnames(x)[2]
      no.y <- FALSE
      y <- x[,2]
      x <- x[,1]
    } else if(ncol(x)==1) {
      if (no.xlab) xlab <- colnames(x)[1]
      x <- x[,1]
    } else stop("Matrix has no columns!")
  }
     if(no.y) {
       ylab <- xlab
       xlab <- "Index"
       y <- x
       x <- 1:length(y)
     }
  
  xy <- NaRV.omit(data.frame(x,y=y))
  x <- xy$x
  y <- xy$y

  mar.orig <- (par.old <- par(c("mar", "las", "mfrow")))$mar
  on.exit(par(par.old))
  w <- (3 + mar.orig[2]) * par("csi") * 2.54
  nf <- layout(matrix(c(1, 2), nc = 2), widths = c(1, lcm(w)))
  x.old <- x
  y.old <- y

  xfac <- is.factor(x.old)
  yfac <- is.factor(y.old)
    
  if(xfac) {
    x <- as.integer(x.old)
  }
  if(yfac) {
    y <- as.integer(y.old)
  }

  if(!(is.vector(x)&is.vector(y)))
    stop("x must be a vector, matrix or data.frame and y must be a vector if present")
  
  par(las=1)
  mar <- mar.orig
  mar[4] <- 1
  par(mar = mar)

  ## drawing labels at
  at.x <- pretty(x,n=nx.lab)
  at.y <- pretty(y,n=ny.lab)

  plot(if (xfac) range(at.x)+0.5*c(-1,+1) else range(at.x),
       if (yfac) range(at.y)+0.5*c(-1,+1) else range(at.y),
       xlab=xlab,
       ylab=ylab,
       type="n",
       axes=FALSE,
       main=main,
       cex.main=cex.main)

  if(is.factor(x.old)) {

    at.x <- at.x[(signif(at.x,dig=1)-at.x)<1e-5]
    axis(1,
         at=at.x,
         labels=abbreviate(levels(x.old)[at.x],minl=2),
         xpd = NA)
  } else {
    axis(1, at=at.x, xpd = NA)
  }

  if(is.factor(y.old)) {
    at.y <- at.y[(signif(at.y,dig=1)-at.y)<1e-5]
    axis(2,
         at=at.y,
         labels=abbreviate(levels(y.old)[at.y],minl=2),
         xpd = NA)
  } else {
    axis(2, at=at.y, xpd = NA)
  }

  zzmax <- Image(x,y,pixs=pixs,zmax=zmax,colramp=colramp, factors=c(xfac,yfac))
  if(is.null(zmax)) zmax <- zzmax
  zmax <- max(zmax,2)
  box()

  ## plotting legend
  mar <- mar.orig
  mar[4] <- mar[2]
  mar[2] <- 1
  par(mar = mar)
  mycol <- c(par("bg"),colramp(zmax))
  
  lev <- 0:length(mycol)
  plot.new()
  plot.window(xlim=c(0, 1), ylim=range(lev,na.rm=TRUE), xaxs="i", yaxs="i")
  if(border) 
    rect(0, lev[-length(lev)], 1, lev[-1], col=mycol)
  else
    rect(0, lev[-length(lev)], 1, lev[-1], col=mycol, border=mycol)
  box()
  ap <- pretty(lev)
  axis(side=4, at=ap+0.5, labels=paste(ap))
  invisible(zmax)
} ## ixyplot

