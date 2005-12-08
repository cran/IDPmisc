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
         pixs = 1, zmax = NULL, ztransf = function(x){x},
         colramp = IDPcolorRamp,
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
    lag plots. The maximum of the number per pixel is delivered by the
    return vallue.}
  \item{ztransf}{Function to transform the counts. The user has to make
    sure that the transformed counts lie in the range [0,zmax], where
    zmax is any positive number (>=2).}
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
if(require(SwissAir)) {
  data(AirQual)

  ## low correlation
  ilagplot(AirQual[,c("ad.O3")],set.lags = 1:9,
         ztransf=function(x){x[x<1] <- 1; log2(x)*10})

  ## high correlation
  Ox <- AirQual[,c("ad.O3","lu.O3","sz.O3")]+
    AirQual[,c("ad.NOx","lu.NOx","sz.NOx")]-
      AirQual[,c("ad.NO","lu.NO","sz.NO")]
  names(Ox) <- c("ad","lu","sz")
  ilagplot(Ox$ad,set.lags = 1:9,
           ztransf=function(x){x[x<1] <- 1; log2(x)*10})

  ## cf. ?AirQual for the explanation of the physical and chemical background
} else print("Package SwissAir is not available")
}

\keyword{hplot}

