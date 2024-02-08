\name{Image}
\alias{Image}

\title{Display the Density of Points in a Scatter Plot by Colors}
\description{
  The density of points in a scatter plot is encoded by color.
}
\usage{
Image(x, y = NULL, pixs = 1, zmax = NULL, ztransf = function(x){x},
      colramp = IDPcolorRamp, factors = c(FALSE, FALSE),
      matrix = FALSE)
}

\arguments{
  \item{x, y}{Coordinates of points whose density is plotted.  If
    \code{x} is a matrix or a data.frame, the first two column are used
    as x and y respectively.  \code{y} must be in this case NULL.
    \code{x} and \code{y} may be numeric or factor variable.}
  \item{pixs}{Size of pixel in x- and y-direction in [mm] on the
    plotting device.  When \code{x} and \code{y} are numeric,
    pixels are square.  When \code{x} and \code{y} are factors or should
    be handled as factors  (see argument \code{factors}), pixels are no
    longer square.  The pixels are enlarged in the dimension in which the
    factors are displayed, so that the rectangular pixels are centered
    at the factor levels.}
  \item{zmax}{Maximum number of counts per pixel in the plot.  When
    \code{NULL}, the density in the scatter plot is encoded from 0
    to maximum number of counts per pixel observed.  \code{zmax} must be
    equal or larger than maximum number of counts found. The maximum
    number of counts per pixel is delivered by the return value.}
  \item{ztransf}{Function to transform the number of counts per
    pixel, which will be mapped by the function in
    \code{colramp} to well defined colors.  The user
    has to make  sure that the transformed density lies in the range
    [0,zmax], where zmax is any positive number (>=2).  For examples see
    \code{\link{ipairs}} and \code{\link{ilagplot}}.}
  \item{colramp}{Color ramp to encode the number of the counts within a
    pixel by color.}
  \item{factors}{Vector of logicals indicating whether \code{x} and / or
    \code{y} should be handled as factors
    independently of their \code{class}.}
  \item{matrix}{Boolean. Should all counts be returned in a xyz-matrix
    or just the maximum.}
}
\details{
  Before calling \code{Image} a plot must have been created by, e.g.,
  calling \code{plot(x,y,type="n")}.  This function ensures
  by default that the pixel has the same size in x- and y-direction.  As
  a drawback, pixels may be unequally spaced, when there are only very
  few distinct (integer) values in x- or y-direction.  When this is the
  case, the corresponding dimension should be declared as a factor.
  (cf. argument \code{factors}). \cr

  This function is based on \pkg{\link{graphics}}
}

\value{
  Maximum number of counts per pixel found (\code{matrix = FALSE}) or
  the full matrix.}

\author{Andreas Ruckstuhl, Rene Locher}

\seealso{ \code{\link{ipairs}}, \code{\link{ilagplot}},
  \code{\link{iplot}}, \code{\link{image}}}

\examples{
plot.default(iris$Species, iris$Petal.Width, xlim=c(0.5,3.5),
             type="n", axes=FALSE)
axis(1, at=1:3, labels=levels(iris$Species))
axis(2)
Image(iris$Species, iris$Petal.Width, pixs=3)

##
x <- rnorm(10000)
y <- rnorm(10000, 10)
plot(x+y, y, type="n")
Image(x+y, y)
abline(a=0, b=1)

## The above can be merged to
iplot(x+y, y, legend=FALSE, oma=c(5, 4, 4, 2) + 0.1)
abline(a=0, b=1)
}

\keyword{aplot}
