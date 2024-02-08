\name{ipanel.smooth}
\alias{ipanel.smooth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Panelplot for itermplot }
\description{
  An example of a useful panel function for huge datasets}
\usage{
ipanel.smooth(x, y = NULL, pixs = 1, zmax = NULL,
              ztransf = function(x) {x},
              colramp = IDPcolorRamp, col = "black", lwd = 2,
              span = 2/3, iter = 3, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x,y}{ Numeric vectors of the same length. }
  \item{pixs}{ Size of pixel in x- and y-direction in [mm] on the
    plotting device. When x and y are numeric, pixels are square.  When x
    and y are factors, pixels are no longer square.  The pixels are
    enlarged in x-direction. }
  \item{zmax}{ Maximum number of counts per Pixel in the plot. When
    NULL, the density in the scatter plot is encoded from 0 pixel to
    maximum number of counts observed.  zmax must be equal or larger than
    maximum number of counts found. }
  \item{ztransf}{ Function to transform the number of counts per pixel,
    which will be mapped by the function in \code{colramp} to well
    defined colors.  The user has to make sure that the transformed
    density lies in the range [0,zmax], where zmax is any positive
    number (>=2).  For examples see \code{\link{ipairs}} and
    \code{\link{ilagplot}}.}
  \item{colramp}{ Color ramp to encode the number of counts within
    a pixel by color. }
  \item{col,lwd}{ Color and line width of the \dQuote{smoothed curve}. }
  \item{span}{ the smoother span. This gives the proportion of points in
    the plot which influence the smooth at each value. Larger values
    give more smoothness. }
  \item{iter}{ The number of robustifying iterations which should be
    performed. Using smaller values of iter will make lowess run
    faster. }
  \item{\dots}{ Other graphical parameters as arguments to the
    \code{\link{lines} function. }}
}

%%\details{}
%%\value{}
%%\references{ }
\author{ Rene Locher }
%%\note{}
\seealso{ \code{\link[stats]{lowess}} }
\examples{
r.lm <- lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width+Species,
           data = iris)
par(mfrow = c(2,2), pty = "s")
itermplot(r.lm, se = TRUE, partial.res = TRUE, smooth = ipanel.smooth,
          lwd.smth = 3, pixs = 2, ask = FALSE)

if (require(SwissAir)) {
  par(mfrow = c(1,1))
  dat <- log(AirQual[,c("ad.O3", "ad.WS")])
  pairs(dat,
        panel = ipanel.smooth)
} else print("Package SwissAir is not available")

}
\keyword{ hplot }

