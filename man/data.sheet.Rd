\name{data.sheet}
\alias{data.sheet}

\title{Coerce a list to a data.frame}
\description{
  Coerces a list with vectors of different length into a data.frame.  Fills the shorter vectors with NA.
}
\usage{
data.sheet(x)
}

\arguments{
  \item{x}{List to be converted.}
}
\details{
  This function is convenient for comparing / controling data in Lists
  whose components should have approximately the same length.
}
\value{
data.frame
}

\author{Thomas Unternaehrer}

\examples{
data.sheet(list(a=1:5,b=1:4))
}

\keyword{manip}

