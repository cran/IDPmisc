"h.image" <-
function(x, y, pixs=1, zmax=NULL, colramp=IDPcolorRamp)
  ## internal functions for iplot, ilagplot, ixyplot  
  ## pixs = Pixelsize in mm
  ## zmax = Maximale Anzahl Punkte pro Rasterquadrat
{
  pixs <- (pixs/10)/2.54
  npix <- round(par("pin")/pixs)
  usr <- par("usr")
  bx <- seq(usr[1],usr[2], length=npix[1]+1)
  by <- seq(usr[3],usr[4], length=npix[2]+1)
  zz <- table(cut(x,b=bx), cut(y, b=by))
  zzmax <- max(zz)
  if(is.null(zmax)) zmax <- zzmax
  ## bg color for 0
  if(zmax<1) {
    zmax <- 1
    par
    warning("zmax must be >= 1. zmax set to 1")
  }
  if(zzmax>zmax)
    stop("Zmax too small! Densiest aereas are out of range!",call. = FALSE)

  image(x=bx, y=by, zz, col=colramp(zmax),
        breaks=seq(0.5,zmax+1,1),  xaxs="r", yaxs="r", add=TRUE)
  box()
  return(zzmax)
} # h.image

