\name{ipairs}
\alias{ipairs}

\concept{large dataset}
\concept{huge dataset}

\title{Image Scatter Plot Matrix for Large Datasets}
\description{
  Produces an image scatter plot matrix of large datasets where the colors
  encode the density of the points in the scatter plots.
}
\usage{
ipairs(x,
       pixs = 1, zmax = NULL, colramp = IDPcolorRamp, border = FALSE, 
       labels, cex.lab = NULL, nlab = 5, minL.lab = 2, pty = "s", 
       main = NULL, cex.main = par("cex.main"), verbose = FALSE, ...)
}

\arguments{
  \item{x}{data.frame or matrix}
  \item{pixs}{Pixel size in mm on the plotting device.}
  \item{zmax}{Maximum counts per Pixel to be plotted. When NULL each
    scatter plot has its individual scale. If a number >= maximum number of
    counts per pixel is supplied, the scale will be identical for all
    scatter plots. The maximum of the number per pixel is delivered by
    the return value.} 
  \item{colramp}{Color ramp to encode the density of the points within a
    pixel.}
  \item{border}{Shall border be drawn between the colors in the legend:
    \code{TRUE} / \code{FALSE}.}  
  \item{labels}{Labels of columns. If NULL the names of x are used.}
  \item{minL.lab}{The minimum length of the abbreviations of factor
    levels, used to label the axes.}
  \item{cex.lab}{Size of the labels. If NULL they are calculated automatically.}
  \item{nlab}{Approximate number of labels on axes.}
  \item{pty}{See Argument \code{pty} in \code{\link{par}}.}
  \item{main}{Titel.}
  \item{cex.main}{Size of title.}
  \item{verbose}{When \code{verbose == TRUE}, the values for
    \code{cex.main} and \code{cex.lab} are returned in addition to \code{zmax}.}
  \item{\dots}{Additional arguments to \code{\link{plot.default}}}
}
\details{
  The idea is similar to \code{\link[hexbin]{plot.hexbin}}. The hexagons are
  better suited to reflect the density of points in a plane than the
  squares used here. Nevertheless squares are, contrary to hexagons,
  invariant to reflexions at the x- \emph{and} y-axis and therefore suited for
  scatter plot matrices and also for plotting factors.

  Code is based on R function \code{\link{pairs}} V1.7.
}

\value{
  Maximum number of counts per Pixel found. Additional elements are
  returned when \code{verbose == TRUE}.
}

\author{Andreas Ruckstuhl, refined by Rene Locher}

\note{When you get the error message "Zmax too small! Densiest aereas
  are out of range!" you must run the function again without specifying
  zmax. The value returned gives you the minimum value allowed for zmax.}

\seealso{\code{\link{ilagplot}}, \code{\link{ixyplot}}, \code{\link{Image}}}

\examples{
zmax <- ipairs(airquality,pixs=2,main="Air Quality")
ipairs(airquality,pixs=2,zmax=zmax,border=TRUE)

zmax <- ipairs(iris,pixs=2)
ipairs(iris,pixs=2,zmax=zmax)
}

\keyword{hplot}

