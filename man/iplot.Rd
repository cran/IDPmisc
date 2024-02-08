\name{iplot}
\alias{iplot}

\concept{large dataset}
\concept{huge dataset}

\title{Image Scatter Plot for Large Datasets}

\description{
  Produces an image scatter plot of large datasets where the colors
  encode the density of the points in the scatter plot.  Works also with factors.
}

\usage{
iplot(x, y = NULL,
      pixs = 1, zmax = NULL, ztransf = function(x){x},
      colramp = IDPcolorRamp, cex = par("cex"),
      main = NULL, d.main = 1, cex.main = par("cex.main"),
      xlab = NULL, ylab = NULL, cex.lab = 1,
      legend = TRUE, d.legend = 1,
      cex.axis = par("cex.axis"), nlab.xaxis = 5, nlab.yaxis = 5,
      minL.axis = 3, las = 1, border = FALSE,
      oma = c(5,4,1,0)+0.1, mgp = c(2,0.5,0)*cex.axis, tcl = -0.3, ...
      )
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
  \item{cex}{Magnification of text relative to default.}
  \item{main}{Title.}
 \item{d.main}{Vertical distance between upper border of scatter plots
    and the title line in multiples of title height.}
  \item{cex.main}{Magnification used for title relative to the current
        setting of cex.}
  \item{xlab}{Label for x-axis.}
  \item{ylab}{Label for y-axis.}
  \item{cex.lab}{Magnification used for text in axis labels relative to
    the current setting of cex}
  \item{legend}{Logical. When \code{FALSE}, no legend is plotted and
    space is saved in figure region. }
  \item{d.legend}{Horizontal distance between right border of
    scatter plot and legend in multiples of title height.}
  \item{cex.axis}{Magnification used for axis annotation
    relative to the current setting of cex. }
  \item{nlab.xaxis, nlab.yaxis}{Approximate number of labels on x- and
    y-axes respectively.}
  \item{minL.axis}{The minimum length of the abbreviations of factor
    levels, used to label the axes ticks.}
  \item{las}{Orientation of labels on axes.}
  \item{border}{Logical. When \code{TRUE}, a border is drawn around the
    individual colors in the legend.}
  \item{oma}{Outer margin. Cf. \code{\link{par}}}
  \item{mgp, tcl}{Cf. \code{\link{par}}}
  \item{\dots}{Additional arguments to \code{\link{par}}}
}

\details{
  The idea of this plot is similar to
  \code{\link[hexbin]{gplot.hexbin}}.  The
  hexagons are better suited to reflect the density of points in a plane
  than the squares used here.  Nevertheless squares are, contrary to
  hexagons, invariant to reflexions at the x- \emph{and} y-axis and
  therefore suited for scatter plot matrices and also for plotting
  factors.
}

\value{
  Maximum number of counts per Pixel found.
}

\author{Andreas Ruckstuhl, Rene Locher}

\note{When you get the error message "Zmax too small! Densiest aereas
  are out of range!" you must run the function again without specifying
  zmax.  The value returned gives you the minimum value allowed for zmax.}

\seealso{\code{\link{ilagplot}}, \code{\link{ipairs}}, \code{\link{Image}}}

\examples{
x <-  rnorm(10000)
y <- atan(rnorm(10000, 0))
iplot(x, y)
iplot(x, pixs = 2)

oma <- c(5,5,0,0)
iplot(iris[,1:2], pixs = 4, oma = oma)
iplot(iris[,"Petal.Width"], iris[,"Species"], pixs = 4, oma = oma)
iplot(x = iris[,"Species"], y = iris[,"Petal.Width"], pixs = 10,border = TRUE,
        xlab = "Species",
        ylab = "Petal Width",
        main = "Iris Species and Petal Width", oma = oma)

iplot(iris$Species, iris$Petal.Width,pixs = 3, minL.axis = 10,
      oma = c(3,6,0,0), mgp = c(4, 1, 0),
      cex.axis = 2, cex.lab = 2, cex.main = 2, main = "Larger fonts")
}

\keyword{hplot}
