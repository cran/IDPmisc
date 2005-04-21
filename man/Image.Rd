\name{Image}
\alias{Image}

\title{Displays the Density of Points in a Scatter Plot by Colors}
\description{
  The density of points in a scatter plot is encoded by color.
}
\usage{
Image(x, y = NULL, pixs = 1, zmax = NULL, colramp = IDPcolorRamp,
      factors = c(FALSE, FALSE))
}

\arguments{
  \item{x, y}{Coordinates of points whose density is plotted. If
    \code{x} ist a matrix or a data.frame, the first two column are used as x
    and y respectively. \code{y} must be in this case NULL. \code{x} and
    \code{y} may be numeric or factor variable.}
  \item{pixs}{Size of pixel in x- and y-direction in [mm] on the
    plotting device. When \code{x} and \code{y} are numeric, 
    pixels are square. When \code{x} and \code{y} are factors or should
    be handled as factors  (see argument \code{factors}), pixels are no
    longer square. The pixels are enlarged in the dimension in which the
    factors are displayed, so that the rectangular pixels are centered
    at the factor levels.}
  \item{zmax}{Maximum counts per Pixel to be plotted. When \code{NULL}
    the density in the scatter plot is encoded from 0 pixel to maximum
    number of pixels found. \code{zmax} must be equal or larger than
    maximum counts found.} 
  \item{colramp}{Color ramp to encode the density of the points within a
    pixel by color.}
  \item{factors}{Vector of logicals indicating whether \code{x} and / or
    \code{y} should be handled as factors 
    independently of their \code{class}.}
}
\details{
  Before calling \code{Image} a plot must have been created by, e.g.,
  calling \code{plot(x,y,type="n")}. This function ensures 
  by default that the pixel has the same size in x- and y-direction. As
  a drawback, pixels may be unequally spaced, when there are only very
  few distinct (integer) values in x- or y-direction. When this is the case, the
  corresponding dimension should be declared as factor. (cf. argument
  \code{factors}).\cr 

  A similar solution for plotting densities is realized in the package
  \pkg{fields}:  
  \code{\link[fields]{image.count}}. The main differences between the
  solution in \pkg{IDPmisc} and \pkg{fields} are the following: In
  \code{Image} the pixel size is defined in mm on the plotting device
  whereas in  \code{\link[fields]{image.count}} the number of pixels in
  x- and y-direction are defined. \code{Image} works on ordinary plots
  whereas  \code{image.count} needs the special plotting
  function \code{\link[fields]{image.plot}}. In addition, \code{Image}
  also works with factors as x- and / or y-coordinates
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
abline(a=0,b=1)

##
plot.default(iris$Species,iris$Petal.Width,
             xlim=c(0.5,3.5),
             type="n",axes=FALSE)
axis(1,at=1:3,lab=levels(iris$Species))
axis(2)
Image(iris$Species,iris$Petal.Width,pixs=3)

## The above can be merged to
ixyplot(iris$Species,iris$Petal.Width,pixs=3, minL.lab=10)

}

\keyword{aplot}
