`ilagplot` <-
function (x,
                      set.lags=1,
                      pixs=1,
                      zmax=NULL,
                      ztransf=function(x){x},
                      colramp=IDPcolorRamp,
                      mfrow=NULL,
                      main = NULL,
                      d.main = 1,
                      cex.main = 2*par("cex.main"),
                      legend = TRUE,
                      d.legend = 1,
                      cex.axis = par("cex.axis"),
                      las = 1,
                      border=FALSE,
                      mar = c(3,3,2,0),
                      oma = c(3,3,1,0),
                      ...)

  ## based on R function lag.plot V1.7
  ## Authors: Andreas Ruckstuhl, Rene Locher
  ## Version 09-02-19
{
  if (!(is.vector(x)|is.ts(x))) stop("x must be a vector or ts\n")
  if(!is.numeric(x)) stop("x must be numeric or ts\n")

  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  par(oma=oma, mar=rep(0,4))

  tot.lags <- length(set.lags)
  if (is.null(mfrow))
    mfrow <- n2mfrow(tot.lags)

  w <- par("cin")[1] * 2.54
  h <- par("cin")[2] * 2.54

  w.legend <- lcm(4*w)
  h.main <- lcm(1*h)
  d.main <- lcm(d.main*1.2*h)
  d.legend <- lcm(d.legend*1.2*h)

  if (!is.null(main) & legend) { ## plot title and legend
      lom <- matrix(1:prod(mfrow), ncol=mfrow[2], byrow=TRUE) + 2
      lom <- rbind(c(rep(1,mfrow[2]),rep(0,2)),rep(0,mfrow[2]+2),
                   cbind(lom,rep(0,mfrow[1]),rep(2,mfrow[1])))

      lo <- layout(lom,
                   width=c(rep(1,mfrow[2]), d.legend, w.legend),
                   height=c(h.main, d.main, rep(1,mfrow[1])),
                   respect=TRUE)
      iplotMain(main,cex.main)
      iplotLegend(colramp,zmax,cex.axis,border,mar=c(mar[1],0,mar[3],3))
  } ## plot title and legend

  if (is.null(main) & legend) { ## plot legend only
      lom <- matrix(1:prod(mfrow), ncol=mfrow[2], byrow=TRUE) + 1
      lom <- cbind(lom,rep(0,mfrow[1]),rep(1,mfrow[1]))

      lo <- layout(lom,
                   width=c(rep(1,mfrow[2]), d.legend, w.legend),
                   height=rep(1,mfrow[1]),
                   respect=TRUE)
      iplotLegend(colramp,zmax,cex.axis,border,mar=c(mar[1],0,mar[3],3))
  }## plot legend only

  if (!is.null(main) & !legend) { ## plot title only
      lom <- matrix(1:prod(mfrow), ncol=mfrow[2], byrow=TRUE) + 1
      lom <- rbind(rep(1,mfrow[2]),
                   rep(0,mfrow[2]),
                   lom)

      lo <- layout(lom,
                   width=rep(1,mfrow[2]),
                   height=c(h.main, d.main, rep(1,mfrow[1])),
                   respect=TRUE)
      iplotMain(main,cex.main)
  } ## plot title only

  if (is.null(main) & !legend) { ## No title, no legend
      lom <- matrix(1:prod(mfrow), ncol=mfrow[2], byrow=TRUE)

      lo <- layout(lom,
                   width=rep(1,mfrow[2]),
                   height=rep(1,mfrow[1]),
                   respect=TRUE)

  } ## No title, no legend

  ## layout.show(lo)
  ## return()

  cntsmax <- 0
  n <- length(x)

  par(mar=mar, las = las, cex.axis=cex.axis, pty="s", ...)
  xl <- range(x,na.rm=TRUE)
  for (ll in set.lags) {
    xx <- x[1:(n - ll)]
    xy <- x[(ll+1):n]
    plot.default(xx, xy, xlim=xl, ylim=xl, xlab="", ylab="",
                 mgp=c(3,1, 0), type="n", las=1, ...)
    mtext(side=3, line=0.5, text=paste("lag",ll))
    cntsmax <- max(cntsmax,
                   Image(x=xx, y=xy, pixs=pixs, zmax=zmax,
                         ztransf=ztransf, colramp=colramp))
  }

  invisible(cntsmax)
} # ilagplot

