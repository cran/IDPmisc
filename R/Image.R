"Image" <-
function(x, y, pixs=1, zmax=NULL, colramp=IDPcolorRamp)
  ## Author: Andreas Ruckstuhl, refined by Rene Locher
  ## Version: 2005-01-18
{
  xy <- na.omit(data.frame(x=x,y=y))
  pixs <- (pixs/10)/2.54
  npix <- round(par("pin")/pixs)
  usr <- par("usr")
  bx <- seq(usr[1],usr[2], length=npix[1]+1)
  by <- seq(usr[3],usr[4], length=npix[2]+1)
  zz <- table(cut(xy$x,b=bx), cut(xy$y, b=by))
  zzmax <- max(zz)

  if(is.null(zmax)) zmax <- zzmax
  if(zmax<1) {
    zmax <- 1
    par
    stop("\nzmax must be >= 1 and
          plot(x,y,...) must have been called before calling this function!")
  }
  if(zzmax>zmax)
    stop("\nZmax too small! Densiest aereas are out of range!",call. = FALSE)

  ## bg color for 0
  image(x=bx, y=by, zz, col=colramp(zmax),
        breaks=seq(0.5,zmax+1,1),  xaxs="r", yaxs="r", add=TRUE)
  box()
  return(zzmax)
} # Image

