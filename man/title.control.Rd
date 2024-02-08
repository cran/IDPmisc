\name{title.control}
\alias{title.control}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Auxilary for Controlling  the Title of a Rose Plot }
\description{
 Auxilary for controlling  the title of a rose plot.
}

\usage{
title.control(text = NULL, cex = 1.5,
              between = if (is.null(text)) 0 else 1)
}

\arguments{
  \item{text}{ Title. }
  \item{cex}{ Size of characters in title in multiples of \code{cex} as
    defined in \code{\link{general.control}}.}
  \item{between}{ Distance between title and label North. Default units
  are \code{grid.control(cyclVar.cex)}. Other units can be defined by
  \code{\link[grid]{unit}}.}
}

\value{
  Returns the arguments conveniently packaged up in a list to supply the
  arguments for the title of \code{\link{plot.rose}}.
}

\author{ Rene Locher }

%% \note{}

\seealso{ \code{\link{plot.rose}}, \code{\link{general.control}} }

\examples{
title.control()
}
\keyword{ dplot }
