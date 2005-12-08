\name{cart2clock}
\alias{cart2clock}

\title{ Converts Cartesian Coordinates to Clock Coordinates }
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

%% \details{
%%   ~~ If necessary, more details than the __description__  above ~~
%% }

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

\keyword{ dplot }
\keyword{ manip }
\keyword{ utilities }
