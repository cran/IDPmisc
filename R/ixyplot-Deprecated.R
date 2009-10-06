### ixyplot.R

ixyplot <- function(x,
                  y=NULL,
                  pixs=1,
                  zmax=NULL,
                  ztransf=function(x){x},
                  colramp=IDPcolorRamp,
                  border=FALSE,
                  xlab=NULL,
                  ylab=NULL,
                  nx.lab=5,
                  ny.lab=5,
                  minL.lab=3,
                  main=NULL,
                  cex.main=par("cex.main"))
  {
    .Deprecated("longtsPlot", package="IDPmisc")
    iplot(x = x,
          y = y,
          pixs=pixs,
          zmax=zmax,
          ztransf=ztransf,
          colramp=colramp,
          border=border,
          xlab=xlab,
          ylab=ylab,
          nx.lab=nx.lab,
          ny.lab=ny.lab,
          minL.lab=minL.lab,
          main=main,
          cex.main=cex.main)
  }

