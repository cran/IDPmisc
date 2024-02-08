\name{IDPcolorRamp}
\alias{IDPcolorRamp}

\title{Color Ramp for Ordered Values}

\description{
  Produces color ramps which change simultanously hues, saturation and
  values as defined in the \code{\link[grDevices]{hsv}} modus.  This
  allows to produce especially smooth transitions from one color to the
  next.  The default color ramp starts with light blue, continues with
  green, yellow, red and ends with dark violet.
}

\usage{
IDPcolorRamp(n,
             colInt = data.frame(h = c(0.47, 0.28, 0.16, 0, 1, 0.8),
                                 s = c(0.31, 0.55, 0.7, 0.8, 0.8, 1),
                                 v = c(1, 1, 1, 1, 1, 0.4)),
             fr = c(0.27, 0.27, 0.27, 0))
}

\arguments{
  \item{n}{Total number of different colors in color ramp.}
  \item{colInt}{Data.frame or matrix with the columns h,s & v
    which defines the Intervals for individual color subramps, with
    nrow(colInt) = nsr+1 with nsr>1.  See details}
  \item{fr}{Fraction of the colors in each of the first nsr-1 subramps.}
}

\details{
  The function distributes the number of colors in the subramps, given
  the fractions fr, as smoothly as possible. The default arguments are
  optimized to most distinct colors possible, also for very small
  \code{n}. There is at least one color in the first and the last
  subramp.\cr
  Definition of hsv code:
  \describe{
  \item{\code{h}}{Hue of hsv-Signal: 0=red, 1/3=green, 2/3=blue, 1=red.}
  \item{\code{s}}{Saturation of hsv-Signal: 0=white, 1=full color.}
  \item{\code{v}}{Value of hsv-Signal: 0=black, 1=full color.}
  }
}

\value{
  A vector of \code{n} colors.
}

\author{Rene Locher}

\note{If there are subramps which are not adjacent in the color space
  (as is here the case for red and violet), you need a virtual subramp
  (here from h=0.00 to h=1.00) with corresponding fraction fr == 0.}

\seealso{\code{\link{showColors}},
  \code{\link[RColorBrewer]{ColorBrewer}}}

\examples{
IDPcolorRamp(10)

## Default IDPcolorRamp in 21 colors
n <- 21
showColors(IDPcolorRamp(n),border=FALSE)

## colorRamp optimized to return at equidistant indices the colors
## light blue, light green, yellow, orange, red, dark violet
## works fine with n > 7
cInt <- data.frame(h = c(0.47, 0.28, 0.16, 0, 1, 0.8),
                   s = c(0.31, 0.55, 0.7, 0.8, 0.8, 1),
                   v = c(1, 1, 1, 1, 1, 0.5))

fr <- c(0.15, 0.25, 0.45, 0.0)
ii <- seq(1,n,length.out=6)

## colors at equidistant indices
showColors(IDPcolorRamp(n, colInt = cInt, fr =fr)[ii], border=FALSE)

## Alternative ramp in 21 colors
showColors(IDPcolorRamp(n, colInt = cInt, fr =fr), border=FALSE)
}

\keyword{color}
