\name{ok}
\alias{ok}
\title{Sets NAs in Logical Objects to FALSE}
\description{
  Sets NAs in Logical Vectors and Matrices to FALSE.
}
\usage{
ok(x)
}

\arguments{
  \item{x}{Logical vector or matrix}
}

\value{
Logical vector or matrix, containing no NAs.
}
\author{Rene Locher}

\examples{
ok(c(FALSE,TRUE,NA,TRUE))
}

\keyword{ manip }

