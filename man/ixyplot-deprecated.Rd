\name{ixyplot}
\alias{ixyplot}


\concept{large dataset}
\concept{huge dataset}

\title{Image Scatter Plot for Large Datasets}

\description{
  Deprecated. Use \code{iplot()} instead!
}

\usage{
ixyplot(x, y = NULL,
        pixs = 1, zmax = NULL, ztransf = function(x){x},
        colramp = IDPcolorRamp, border = FALSE,
        xlab = NULL, ylab = NULL, nx.lab = 5, ny.lab = 5, minL.lab = 3,
        main = NULL, cex.main = par("cex.main"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x,y}{Coordinates of points whose density is to be plotted.  If
    \code{x} ist a matrix or a data.frame, the first two column are used
    as x and y respectively.  \code{y} must be in this case NULL.
    \code{x} and \code{y} may be numeric or factor variable. }
  \item{pixs}{Pixelsize in mm.}
  \item{zmax}{Maximum number of counts per pixel in the plot.  When
    \code{NULL}, the density in the scatter plot is encoded from 0 
    to maximum number of counts per pixel observed.  \code{zmax} must be
    equal or larger than maximum number of counts found.  The maximum
    number of counts per pixel is delivered by the return value.}
  \item{ztransf}{Function to transform the number of counts per
    pixel, which will be mapped by the function in  \code{colramp} to
    well defined colors.  The user 
    has to make  sure that the transformed density lies in the range
    [0,zmax], where zmax is any positive number (>=2).  For examples see
    \code{\link{ipairs}} and \code{\link{ilagplot}}.}  
  \item{colramp}{Color ramp to encode the number of counts within a
    pixel.}
  \item{border}{Shall border be drawn between the colors in the legend:
    \code{TRUE} / \code{FALSE}.}
  \item{xlab}{Label for x-axis.}
  \item{ylab}{Label for y-axis.}
  \item{nx.lab}{Approximate number of labeled ticks on x-axis.}
  \item{ny.lab}{Approximate number of labeled ticks on y-axis.}
  \item{minL.lab}{The minimum length of the abbreviations of factor
        levels, used to label the axes.}
  \item{main}{Title.}
  \item{cex.main}{Size of title relative to \code{par("cex")}}
}

\details{
  The idea of this plot is similar to
  \code{\link[hexbin]{plot.hexbin}}.  The 
  hexagons are better suited to reflect the density of points in a plane
  than the squares used here.  Nevertheless squares are, contrary to
  hexagons, invariant to reflexions at the x- \emph{and} y-axis and
  therefore suited for scatter plot matrices and also for plotting
  factors.
  \crThis function is based on \pkg{\link{graphics}}
}

\value{
  Maximum number of counts per Pixel found.
}

\author{Andreas Ruckstuhl, Rene Locher}

\note{When you get the error message "Zmax too small! Densiest aereas
  are out of range!" you must run the function again without specifying
  zmax.  The value returned gives you the minimum value allowed for zmax.}

\seealso{\code{\link{ilagplot}}, \code{\link{ipairs}}, \code{\link{Image}}}

\keyword{hplot}
