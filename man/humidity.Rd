\name{humidity}
\alias{hr}
\alias{pw}
\alias{pw.ai}
\alias{pw.aw}
\alias{Td}
\alias{Td.aw}
\alias{Tf.ai}

\title{
  Converting Humidity Measures into Each Other
}
\description{
  Converting dew point (of water in air) into vapor pressure or relative humidity and vice versa.
}
\usage{
hr(T, Td, warn = TRUE)
pw(Td, warn = TRUE)
pw.ai(Td, warn = TRUE)
pw.aw(Td, warn = TRUE)
Td(pw = NULL, T = NULL, hr = NULL, warn = TRUE)
Td.aw(pw = NULL, T = NULL, hr = NULL, warn = TRUE)
Tf.ai(pw = NULL, T = NULL, hr = NULL, warn = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hr}{
    Relative humidity in \eqn{[\textnormal{\%}]}.
  }
  \item{T}{
    Temperature of air in \eqn{[^\circ \textnormal{C}]}.
  }
  \item{Td}{
    Dew point of air in \eqn{[^\circ \textnormal{C}]}.
  }
  \item{pw}{
    Saturation vapour pressure in \eqn{[\textnormal{hPa}]}.
  }
  \item{warn}{
    When \code{TRUE} and arguments are out of range, a warning is thrown.
  }
}
\details{
All vapour pressures correspond to pure water vapour and are not
  adjusted to water vapour in air.
}
\value{
  \code{hr} transforms dew point above ice / water at a certain temperature into
  relative humidity (\eqn{-65^\circ\textnormal{C} <= T <=
    60^\circ\textnormal{C}}).\cr
  \code{pw} transforms dew point into saturation vapour pressure above ice
  (\eqn{-65^\circ\textnormal{C} <= T <=
    0^\circ\textnormal{C}})  and above water (\eqn{0^\circ\textnormal{C} < T <=
    60^\circ\textnormal{C}}) respectively.\cr
  \code{pw.ai} transforms dew point into saturation vapour
  pressure above ice (\eqn{-65^\circ\textnormal{C} <= T <=
    0^\circ\textnormal{C}}).\cr
  \code{pw.aw} transforms dew point into saturation vapour
  pressure above liquid water (\eqn{-45^\circ\textnormal{C} <= T <=
    60^\circ\textnormal{C}}).\cr
  \code{Td} transforms vapour pressure or relative humidity and
  temperature into frost point above ice (\eqn{-65^\circ\textnormal{C} <= T <=
    0^\circ\textnormal{C}}) and dew point above water (\eqn{0^\circ\textnormal{C} < T <=
    60^\circ\textnormal{C}}) respectively.\cr
  \code{Td.aw} transforms vapour pressure or relative humidity and
  temperature into dew point above water (\eqn{-45^\circ\textnormal{C} <= T <=
    60^\circ\textnormal{C}}).\cr
  \code{Tf.ai} transforms vapour pressure or relative humidity and
  temperature into frost point above ice (\eqn{-65^\circ\textnormal{C} <= T <=
    0^\circ\textnormal{C}}).
}
\references{
 Guide to Meteorological Instruments and Methods of Observation, WMO,
 WMO-No. 8, Seventh edition, 2008, updated 2010, 1.4-29 Annex 4.B
}
\author{
Rene Locher
}

\examples{
 pw(Td = c(-65, -45, 0, 30, 60))
 Td(pw = pw(c(-20, 0, 20)))

 hr(T = c(20, 30, 40), Td = c(0, 20, 30))
 ## [1] 26.20257 55.09561 57.46519

 hr(T = seq(0, 60, 5), Td = 0)
}

\keyword{ arith }% use one of  RShowDoc("KEYWORDS")

