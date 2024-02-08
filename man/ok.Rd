\name{ok}
\alias{ok}
\title{Sets NAs in Logical Objects to FALSE}
\description{
  Sets NAs in logical vectors and matrices to FALSE.  This is especially
  useful for conditional selections of data when the variables the
  condition is based on contain \code{NA}s.
}
\usage{
ok(x)
}

\arguments{
  \item{x}{Logical vector or matrix}
}

\value{
Logical vector or matrix, containing no \code{NA}s.
}
\author{Rene Locher}

\examples{
ok(c(FALSE,TRUE,NA,TRUE))
}

\keyword{ manip }

