\name{key.control}
\alias{key.control}

\title{ Auxilary for Controlling the Appearance of the Legend of a Rose
  Plot }

\description{
  Defines the appearance of the legend of a Rose Plot.
}

\usage{
key.control(plot = TRUE, lab = NULL, title = NULL, between = 0)
}

\arguments{
  \item{plot}{ Defines whether legend should be drawn.  When
    \code{FALSE}, the full width of the viewport is used for the rose itself.  }
  \item{lab, title}{ Labels and title of
    legend. When \code{lab = NULL}, labels are extracted from the
    names of the slot \code{x@rho} of \code{\link{rose}} object. }
  \item{between}{Distance between label East and left edge of legend in
    \code{cex} as defined in \code{\link{general.control}}}
}

\value{
  Returns the arguments conveniently packaged up in a list to supply the
  arguments for the legend in \code{\link{plot.rose}}
}

\author{ Rene Locher }

%% \note{}

\seealso{ \code{\link{plot}} }

\examples{
key.control()
}

\keyword{ dplot }
