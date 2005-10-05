"ilagplot" <-
function (x,
                      set.lags=1,
                      pixs=1,           
                      zmax=NULL,
                      ztrans=function(x){x},
                      colramp=IDPcolorRamp,  
                      mfrow=NULL,
                      border=FALSE,
                      main=NULL,         
                      cex.main=2*par("cex.main"), ## don't understand factor 2!
                      ...) 
  
  ## based on R function lag.plot V1.7
  ## Authors: Andreas Ruckstuhl, refined by Rene Locher
  ## Version 31-01-05
{
  if (!(is.vector(x)|is.ts(x))) stop("\nx must be a vector or ts")
  if(!is.numeric(x)) stop("\nx must be numeric or ts")
     
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  tot.lags <- length(set.lags)
  if (is.null(mfrow)) 
    mfrow <- n2mfrow(tot.lags)
  lom <- matrix(1:prod(mfrow), ncol=mfrow[2], byrow=TRUE) + 4
  lom <- rbind(c(0,rep(3,mfrow[2]),0),
               cbind(rep(2,mfrow[1]),lom,rep(4,mfrow[1])),
               c(0,rep(1,mfrow[2]),0))
  w <- 3 * (par("cin")[2]*0.66) * 2.54

  lo <- layout(lom,
               width=c(lcm(w),rep(1,mfrow[2]), lcm(2.5*w)),
               height=c(lcm(w),rep(1,mfrow[1]), lcm(w)),
               respect=TRUE)
  ## layout.show(lo)
  ## return()
  LegendAndTitle(main,cex.main,border,colramp,zmax)
   
  cntsmax <- 0
  n <- length(x)
  
  par(mar=c(2.5, 2.5, 2.5, 0.5),pty="s")
  xl <- range(x,na.rm=TRUE)
  for (ll in set.lags) {
    xx <- x[1:(n - ll)]
    xy <- x[(ll+1):n]
    plot.default(xx, xy, xlim=xl, ylim=xl, xlab="", ylab="",
                 mgp=c(3,1, 0), type="n", las=1, ...)
    mtext(side=3, line=0.5, text=paste("lag",ll))
    cntsmax <- max(cntsmax,
                   Image(x=xx, y=xy, pixs=pixs, zmax=zmax, ztrans=ztrans,
                         colramp=colramp)) 
  }
  
  invisible(cntsmax)
} # ilagplot

