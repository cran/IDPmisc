\name{inf.omit}
\alias{inf.omit}

\title{Handles NA, NaN, Inf and -Inf Values in Objects}

\description{
  Omits observations with NA, NaN, Inf and -Inf Values when object is a
  vector, a data.frame or a matrix.
}

\usage{
inf.omit(x)
}

\arguments{
  \item{x}{\code{Vector}, \code{data.frame} or \code{matrix}}
}

\value{
  Returns objects in the same way as \code{\link{na.omit}} does. 
}

\author{ Rene Locher }

\seealso{\code{\link{na.omit}}}

\examples{
inf.omit(c(1,3,NaN,NA,1/0,-1/0,9))
inf.omit(data.frame(RowNr=letters[1:7],Value=c(1,3,NaN,NA,1/0,-1/0,9)))

M <- matrix(c(1,3,5,10,NaN,NA,1/0,-1/0,9:12),ncol=2)
M
inf.omit(M)
}

\keyword{ manip }

