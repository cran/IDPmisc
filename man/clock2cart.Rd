\name{clock2cart}
\alias{clock2cart}
\concept{conversion}

\title{ Convert Clock Coordinates to Cartesian Coordinates }
\description{
  Converts clock coordinates (\code{rho}, \code{phi}) to cartesian
  coordinates (\code{x}, \code{y}).
}
\usage{
clock2cart(rho, phi, circle)
}

\arguments{
  \item{rho}{ Distance of point from center of coordinate system.  }
  \item{phi}{ Angle between North (12 o' clock), center and the point,
    measured clockwise. }
  \item{circle}{ Defines the full circle in the units of \code{phi}. }
}

\details{
  Be aware that in clock coordinates and polar coordinate the angle phi
  is differently defined!
}


\value{
  Data frame with
  \item{\code{x}, \code{y} }{Cartesian coordinates.}
}

%% \references{ ~put references to the literature/web site here ~ }

\author{ Rene Locher }

%%\note{ }

\seealso{ \code{\link{cart2clock}}, \code{\link{rose-class}} }

\examples{
## an easy way to plot a circle
xy <- clock2cart(rho=rep(1,33),phi=seq(0,to=360,length.out=33),circle=360)
plot(xy)
}

\keyword{ dplot }
\keyword{ manip }
\keyword{ utilities }
