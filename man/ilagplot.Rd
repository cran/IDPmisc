\name{ilagplot}
\alias{ilagplot}

\concept{large dataset}
\concept{huge dataset}

\title{Image Lag Plot Matrix for Large Time Series}
\description{
  Produces an image lag plot matrix of large timeseries where the colors
  encode the density of the points in the lag plots.
}

\usage{
ilagplot(x, set.lags = 1,
         pixs = 1, zmax = NULL, colramp = IDPcolorRamp,
         mfrow = NULL, border = FALSE,
         main = NULL, cex.main = 2 * par("cex.main"), ...)
}

\arguments{
  \item{x}{ts object or ordinary vector}
  \item{set.lags}{vector of lags to be displayed}
  \item{pixs}{Pixel size in mm}
  \item{zmax}{Maximum counts per Pixel to be plotted. When NULL each
    lag plot has its individual scale. If a number >= maximum number of
    counts per pixel is supplied, the scale will be identical for all
    lag plots. The maximum of the number per pixel is delivered by the return vallue.}
  \item{colramp}{Color ramp to encode the density of the points within a
  pixel}
  \item{mfrow}{See Argument \code{mfrow} in \code{\link{par}}}
  \item{border}{Shall border be drawn between the colors in the legend:
    \code{TRUE} / \code{FALSE}}
  \item{main}{Title}
  \item{cex.main}{Character size of title}
  \item{\dots}{Additional arguments to \code{\link{plot.default}}}
}

\details{
 Code is based on R function \code{\link{lag.plot}} V1.7.
}

\value{
  Maximum number of counts per Pixel found.
}

\author{Andreas Ruckstuhl, refined by Rene Locher}

\note{When you get the error message "Zmax too small! Densiest aereas
  are out of range!" you must run the function again without specifying
  zmax. The value returned gives you the minimum value allowed for zmax.}

\seealso{\code{\link{ipairs}}, \code{\link{ixyplot}},
    \code{\link{Image}}}
  
\examples{
zmax <- ilagplot(sunspot.month, pixs=2, set.lags=1:9,
                 mfrow=c(3,3),border=FALSE,main="Test")
ilagplot(sunspot.month, pixs=2, zmax=zmax, set.lags=1:6,
         mfrow=c(2,3), border=FALSE)

x <-  rnorm(100000)
ilagplot(x+c(x[-1],0)+c(x[-(1:2)],0,0)+c(x[-(1:3)],0,0,0),
         set.lags=1:4)
}

\keyword{hplot}

