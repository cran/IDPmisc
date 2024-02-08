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
         colramp = IDPcolorRamp, mfrow=NULL, cex=par("cex"),
         main = NULL, d.main = 1, cex.main = 1.5*par("cex.main"),
         legend = TRUE, d.legend = 1,
         cex.axis = par("cex.axis"), las = 1,
         border=FALSE, mar = c(2,2,2,0), oma = rep(0,4)+0.1,
         mgp = c(2,0.5,0)*cex.axis, tcl = -0.3, ...)
}

\arguments{
  \item{x}{ts object or ordinary vector}
  \item{set.lags}{vector of lags to be displayed}
  \item{pixs}{Pixel size in mm}
  \item{zmax}{Maximum counts per Pixel to be plotted.  When NULL each
    lag plot has its individual scale.  If a number >= maximum number of
    counts per pixel is supplied, the scale will be identical for all
    lag plots.  The maximum of the number per pixel is delivered by the
    return value. Beware: \code{zmax} has its meaning only for ilagplots
    with identical settings for main, legend, mar and oma!}
  \item{ztransf}{Function to transform the counts.  The user has to make
    sure that the transformed counts lie in the range [0,zmax], where
    zmax is any positive number (>=2).}
  \item{colramp}{Color ramp to encode the density of the points within a
  pixel}
  \item{mfrow}{See Argument \code{mfrow} in \code{\link{par}}}
  \item{cex}{See Argument \code{cex} in \code{\link{par}}}
  \item{main}{Title}
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
  \item{las}{Orientation of labels on axes.}
  \item{border}{Logical. When \code{TRUE}, a border is drawn around the
    individual colors in the legend.}
  \item{mar, oma}{Margin and outer margin respectively.
    Cf. \code{\link{par}}}
  \item{mgp, tcl}{Cf. \code{\link{par}}}
  \item{\dots}{Additional arguments to \code{\link{par}}}
}

\details{
  Code is based on R function \code{\link{lag.plot}} V1.7.
  Tip: Legend looks  better when \code{mar} is defined symmetrically.
}

\value{
  Maximum number of counts per Pixel found.
}

\author{Andreas Ruckstuhl, refined by Rene Locher}

\note{When you get the error message "Zmax too small! Densiest aereas
  are out of range!" you must run the function with identical parameters
  but without specifying zmax. The value returned gives you the minimum
  value allowed for zmax.}

\seealso{\code{\link{ipairs}}, \code{\link{iplot}},
    \code{\link{Image}}}

  \examples{
if(require(SwissAir)) {
  data(AirQual)

  ilagplot(AirQual[,c("ad.O3")],set.lags = 1:4,
           ztransf = function(x){x[x<1] <- 1; log2(x)},
           main = "Low correlation")

  Ox <- AirQual[,c("ad.O3","lu.O3","sz.O3")]+
    AirQual[,c("ad.NOx","lu.NOx","sz.NOx")]-
      AirQual[,c("ad.NO","lu.NO","sz.NO")]
  names(Ox) <- c("ad","lu","sz")
  ilagplot(Ox$ad,set.lags = 1:4,
           ztransf = function(x){x[x<1] <- 1; log2(x)},
           main = "High correlation")

  ## cf. ?AirQual for the explanation of the physical
  ## and chemical background
} else print("Package SwissAir is not available")
}

\keyword{hplot}

