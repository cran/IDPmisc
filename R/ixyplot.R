"ixyplot" <-
function(x,
                    y=NULL,
                    pixs=1,
                    zmax=NULL,
                    colramp=IDPcolorRamp,
                    border=FALSE,
                    xlab=NULL,
                    ylab=NULL,
                    main=NULL,
                    cex.main=par("cex.main"))
  ## Produces an image scatter plot of a large 2d dataset.
  
  ## based on R function lag.plot V1.7
  ## Authors: Andreas Ruckstuhl, refined by René Locher
  ## Version 25-01-05
{
  if(is.null(xlab)) xlab <- deparse(substitute(x))
  if(is.null(ylab)) ylab <- deparse(substitute(y))

  xdim <- dim(x)
  ydim <- dim(y)
  ## coercing 1d matrix into vector
  if(!is.null(xdim)&&xdim[2]==1) x <- as.vector(x)
  if(!is.null(ydim)&&ydim[2]==1) y <- as.vector(y)

  if(is.null(y)&is.vector(x)) {
    x.old <- x
    x <- 1:length(x)
    y <- x.old
    ylab <- xlab
    xlab <- "Index"
  } else if(is.null(y)&length(dim(x))==2) {
    y<-x[,2]
    x <- x[,1]
    xlab="x"
    ylab="y"
  }

  mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
  on.exit(par(par.orig))
  w <- (3 + mar.orig[2]) * par("csi") * 2.54
  nf <- layout(matrix(c(1, 2), nc = 2), widths = c(1, lcm(w)))
  x.old <- x
  y.old <- y

  if(is.factor(x.old)) {
    x <- as.integer(x.old)
  }
  if(is.factor(y.old)) {
    y <- as.integer(y.old)
  }
  
  if(!(is.vector(x)&is.vector(y)))
    stop("x must be a vector, matrix or data.frame and y must be a vector if present")
  
  par(las=1)
  mar <- mar.orig
  mar[4] <- 1
  par(mar = mar)
  plot(range(x),range(y),
       xlab=xlab,
       ylab=ylab,
       type="n",
       axes=FALSE,
       main=main,
       cex.main=cex.main)

  ## drawing axes
  at <- pretty(x,n=5)

  if(is.factor(x.old)) {
    at <- at[(signif(at,dig=1)-at)<1e-3]
    axis(1,
         at=at,
         labels=abbreviate(levels(x.old),minl=2),
         xpd = NA)
  } else {
    axis(1, at=at, xpd = NA)
  }

  at <- pretty(y,n=5)
  if(is.factor(y.old)) {
    at <- at[(signif(at,dig=1)-at)<1e-3]
    axis(2,
         at=at,
         labels=abbreviate(levels(y.old),minl=2),
         xpd = NA)
  } else {
    axis(2, at=at, xpd = NA)
  }
  
  pixs <- (pixs/10)/2.54
  npix <- round(par("pin")/pixs)
  usr <- par("usr")
  bx <- seq(usr[1],usr[2], length=npix[1]+1)
  by <- seq(usr[3],usr[4], length=npix[2]+1)
  zz <- table(cut(x,b=bx), cut(y, b=by))

  zzmax <- max(zz)
  if(is.null(zmax)) zmax <- zzmax else
  if(zmax>=zzmax) stop("Zmax too small! Densiest aereas are out of range!")
  image(x=bx, y=by, zz, col=colramp(zmax),
        breaks=seq(0.5,zmax+1,1),
        xaxs="r", yaxs="r", add=TRUE)
  box()

  ## plotting legend
  mar <- mar.orig
  mar[4] <- mar[2]
  mar[2] <- 1
  par(mar = mar)
  mycol <- {if(is.null(zmax)) {
               c(par("bg"),colramp(zmax))
               if(zmax<1) {
                 zmax <- 1
                 warning("zmax must be >= 1. zmax set to 1")
               }
             }
            else {
               if(zmax<1) {
                 zmax <- 1
                 warning("zmax must be >= 1. zmax set to 1")
               }
               if(zzmax>zmax)
                 warning("Zmax too small! Densiest aereas are uncolored!")
               c(par("bg"),colramp(zmax))
            }
          }
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
  return(zzmax)
} ## ixyplot

