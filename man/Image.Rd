\name{Image}
\alias{Image}

\title{Plots the Density of Points in Scatter Plot as Image}
\description{
  The density of points in a scatter plot is encoded by color.
}
\usage{
Image(x, y, pixs = 1, zmax = NULL, colramp = IDPcolorRamp)
}

\arguments{
  \item{x, y}{Coordinates of points whose density should be plotted.}
  \item{pixs}{Size of pixel in mm on the plotting device.}
  \item{zmax}{Maximum counts per Pixel to be plotted. When NULL the density
    in the scatter plot is encoded from 0/pixel to maximum/pixel
    found. \code{zmax} must be equal or larger than maximum counts found.}
  \item{colramp}{Color ramp to encode the density of the points within a
    pixel.}
}
\details{
  Before calling \code{Image} a plot must have been created by, for
  example, calling \code{plot(x,y,type="n")}. A similar solution for
  plotting densities is realized in the package \pkg{fields}: 
  \code{\link[fields]{image.count}}. The main differences between the
  solution in \pkg{IDPmisc} and \pkg{fields} are the following: In
  \code{Image} the pixel size is defined in mm on the plotting device
  whereas in  \code{\link[fields]{image.count}} the number of pixels in
  x- and y-direction are defined. \code{Image} works on ordinary plots
  whereas  \code{image.count} needs the special plotting
  function \code{\link[fields]{image.plot}}. 
}

\value{
  Maximum number of counts per Pixel found.}

\author{Andreas Ruckstuhl, refined by Rene Locher}

\seealso{ \code{\link{ipairs}}, \code{\link{ilagplot}},
  \code{\link{ixyplot}}, \code{\link{image}},
  \code{\link[fields]{image.count}}} 

\examples{
x <- rnorm(10000)
y <- rnorm(10000,10)
plot(x+y,y,type="n")
Image(x+y,y)
}

\keyword{aplot}
