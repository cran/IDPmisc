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
       pixs = 1, zmax = NULL, ztransf=function(x){x},
       colramp = IDPcolorRamp, cex = par("cex"),
       lab.diag, cex.diag = NULL,
       main = NULL, d.main = 1.5, cex.main = 1.5*par("cex.main"),
       legend = TRUE, d.legend = 1.5, cex.axis = 0.8*par("cex.axis"),
       nlab.axis = 5, minL.axis = 2, las = 1, border = FALSE,
       mar = rep(0,4), oma = c(3,3,1,0), mgp = c(2,0.5,0)*cex.axis,
       tcl = -0.3, ...)
}

\arguments{
  \item{x}{data.frame or matrix}
  \item{pixs}{Pixel size in mm on the plotting device.}
  \item{zmax}{Maximum number of counts per pixel in the plot.  When NULL,
    each scatter plot has its individual scale.  If a number >= maximum
    number of counts per pixel is supplied, the scale will be identical
    for all scatter plots.  The maximum number of counts per pixel is
    delivered by the return value.}
  \item{ztransf}{Function to transform the counts per
    pixel, which will be mapped by the function in
    \code{colramp} to well defined colors. The user has to make
    sure that the transformed counts lie in the range [0,zmax], where
    zmax is any positive number (>=2).}
  \item{colramp}{Color ramp to encode the number of counts within a
    pixel.}
  \item{cex}{See Argument \code{cex} in \code{\link{par}}}
  \item{lab.diag}{Labels of columns, written into the diagonal of the
    matrix. When  \code{NULL}, the names of x are used.}
  \item{cex.diag}{Magnification used for text in diagonal  relative to
    the current setting of cex.
    When  \code{NULL}, they are calculated automatically.}
  \item{main}{Titel. When \code{NULL}}
  \item{d.main}{Vertical distance between upper border of scatter plots
    and the title line in multiples of title height.}
  \item{cex.main}{Magnification used for title relative to the current
        setting of cex.}
  \item{legend}{Logical. When \code{FALSE}, no legend is plotted and
    space is saved in figure region. }
  \item{d.legend}{Horizontal distance between right border of scatter
    plots and legend in multiples of title height.}
  \item{cex.axis}{Magnification used for axis annotation
    relative to the current setting of cex. }
  \item{nlab.axis}{Approximate number of labels on axes.}
  \item{minL.axis}{The minimum length of the abbreviations of factor
    levels, used to label the axes ticks.}
  \item{las}{Orientation of labels on axes.}
  \item{border}{Logical. When \code{TRUE}, a border is drawn around the
    individual colors in the legend.}
  \item{mar, oma}{Margin and outer margin respectively.
    Cf. \code{\link{par}}}
  \item{mgp, tcl}{Cf. \code{\link{par}}}
  \item{\dots}{Additional arguments to \code{\link{par}}}
}
\details{
  The idea is similar to \code{\link[hexbin]{gplot.hexbin}}.  The
  hexagons are better suited to reflect the density of points in a plane
  than the squares used here.  Nevertheless squares are, contrary to
  hexagons, invariant to reflexions at the x- \emph{and} y-axis and
  therefore better suited for scatter plot matrices and also for
  plotting factors.

  The code is based on R function \code{\link{pairs}} V1.7.
}

\value{
  Maximum number of counts per Pixel found.  Additional elements are
  returned when \code{verbose == TRUE}.
}

\author{Andreas Ruckstuhl, Rene Locher}

\note{When you get the error message "Zmax too small! Densiest aereas
  are out of range!" you must run the function with identical parameters
  but without specifying zmax. The value returned gives you the minimum
  value allowed for zmax.}

\seealso{\code{\link{ilagplot}}, \code{\link{iplot}},
  \code{\link{Image}}}

\examples{
## Small numbers of different values are plotted nicer
## when converted to factors
AQ <- airquality
AQ$Month <- as.factor(AQ$Month)

zmax <- ipairs(AQ, pixs=2, main="Air Quality")
ipairs(AQ, pixs=2, zmax=zmax, main="Air Quality",border=TRUE)

## example with factors
ipairs(iris,pixs=2)

## a really huge dataset
\dontrun{
  if(require(SwissAir)) {
    data(AirQual)

    ## low correlation
    dev.new()
    ipairs(AirQual[,c("ad.O3","lu.O3","sz.O3")],
    ztransf=function(x){x[x<1] <- 1; log2(x)})
    
    dev.new()
    ipairs(AirQual[,c("ad.NO","lu.NO","sz.NO")],
    ztransf=function(x){x[x<1] <- 1; log2(x)})
    
    ## high correlation
    Ox <- AirQual[,c("ad.O3","lu.O3","sz.O3")]+
    AirQual[,c("ad.NOx","lu.NOx","sz.NOx")]-
    AirQual[,c("ad.NO","lu.NO","sz.NO")]
    names(Ox) <- c("ad","lu","sz")
    dev.new()
    ipairs(Ox, ztransf=function(x){x[x<1] <- 1; log2(x)})

    ## cf. ?AirQual for the explanation of the physical and
    ## chemical background
  } else print("Package SwissAir is not available")
}
}

\keyword{hplot}

