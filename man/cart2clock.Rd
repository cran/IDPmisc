\name{cart2clock}
\alias{cart2clock}
\concept{conversion}

\title{ Convert Cartesian Coordinates to Clock Coordinates }
\description{
  Converts cartesian coordinates (\code{x}, \code{y} to clock
  coordinates (\code{rho}, \code{phi})
}

\usage{
cart2clock(x, y, circle)
}

\arguments{
  \item{x, y}{ Cartesian coordinates. }
  \item{circle}{  Defines the full circle in the units of \code{phi}. }
}

\details{
  Be aware that in clock coordinates and polar coordinate the angle phi
  is differently defined!
}

\value{
Data frame with
  \item{rho}{ Distance of point from center of coordinate system.  }
  \item{phi}{ Angle between North (12 o' clock), center and the point,
    measured clockwise. }
}

%% \references{ ~put references to the literature/web site here ~ }

\author{ Rene Locher }

%%\note{ ~~further notes~~ }

\seealso{ \code{\link{clock2cart}}, \code{\link{rose-class}} }

\examples{
## convert clock coordinates to cartesian coordinates
xy <- clock2cart(rho=rep(1,33),phi=seq(0,to=360,length.out=33),circle=360)

## convert the cartesian coordinates back to clock coordinates
rhophi <- cart2clock(xy$x,xy$y,circle=360)

round(clock2cart(rhophi,circle=360)-xy)
## QED
}

\keyword{ dplot }
\keyword{ manip }
\keyword{ utilities }
