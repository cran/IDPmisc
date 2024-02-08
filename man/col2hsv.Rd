\name{col2hsv}
\alias{col2hsv}

\title{
  Convert Color to hsv Code
}

\description{
  Converts color names or values to hsv code.
}

\usage{
col2hsv(col)
}

\arguments{
  \item{col}{Vector of color code.}
}

\value{
  Matrix with the rows
  \item{h}{Hue of hsv-Signal: 0=red, 1/3=green, 2/3=blue, 1=red.}
  \item{s}{Saturation of hsv-Signal: 0=white, 1=full color.}
  \item{v}{Value of hsv-Signal: 0=black, 1=full color.}
}

\author{Rene Locher}

\seealso{\code{\link[grDevices]{col2rgb}}, \code{\link[grDevices]{rgb2hsv}}, \code{\link[grDevices]{hsv}}}
\examples{
col2hsv(c("blue","green","red"))
}

\keyword{color}
\keyword{dplot}

