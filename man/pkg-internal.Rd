\name{internals}
\alias{LegendAndTitle}

\title{Internal Functions}
\description{
  These functions are for internal use only and not documented further.
}
\usage{
LegendAndTitle(main, cex.main, border, colramp, zmax)

plotdat(rho, cyclVar, circle, transf, vp, general, grid, title)

plot.onepage(y1, y2, names1, names2, startP, upf, fpp, overlap, ann.x,
ann.dx, tick.dx, ann.ny, xlab, y1lab, y2lab, y1.col, y2.col, y2.lab,
cex.lab, y1lim, y2lim, aty1, aty2, lty1, lty2, lwd1, lwd2, col1, col2,
leg, leg.y1nam, leg.y2nam, leg.ncol, leg.cex = 1.5, h1, h2, h1.col,
h2.col, main, main.cex, mgp, oma, cex, type)

rose.grob(pdat, general, grid, title)
}

\details{
  \tabular{ll}{
    Function \tab used in\cr
    \code{LegendAndTitle}
    \tab \code{\link{ipairs}}, \code{\link{ilagplot}}\cr
    \code{plotdat} \tab \code{\link{plot.rose}}\cr
    \code{rose.grob} \tab \code{\link{plot.rose}}\cr
    \code{} \tab \code{\link{plot.rose}}\cr
  }
  
}

\author{Rene Locher}

\keyword{internal}

