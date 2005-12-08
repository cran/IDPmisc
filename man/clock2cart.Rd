\name{clock2cart}
\alias{clock2cart}

\title{ Converts Clock Coordinates to Cartesian Coordinates }
\description{
  Converts clock coordinates (\code{rho}, \code{phi}) to cartesian
  coordinates (\code{x}, \code{y}.
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

%% \details{
%%   ~~ If necessary, more details than the __description__  above ~~
%% }
%% 

\value{
  Data frame with 
  \item{\code{x}, \code{y} }{Cartesian coordinates.}
}

%% \references{ ~put references to the literature/web site here ~ }

\author{ Rene Locher }

%%\note{ }

\seealso{ \code{\link{cart2clock}}, \code{\link{rose-class}} }

\examples{
xy <- clock2cart(5,80,360)
xy
cart2clock(xy[1],xy[2],360)

cart2clock(0,1,360)
cart2clock(1,1,360)
}

\keyword{ dplot }
\keyword{ manip }
\keyword{ utilities }
