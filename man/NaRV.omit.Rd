\name{NaRV.omit}
\alias{NaRV.omit}

\title{Omit Observations with NA, NaN, Inf and -Inf Values}
\description{
  Omits observations with values which a not regular (=Not a Regular
  Value) when object is a vector, a factor, a data.frame or a matrix. 
}
\usage{
NaRV.omit(x)
}

\arguments{
  \item{x}{\code{Vector}, \code{data.frame} or \code{matrix}}
}
\details{
  Irregular values are defined as \code{NA}, \code{NaN}, \code{Inf} and
  \code{-Inf} Values in numerics and \code{NA} in factors and
  characters.  
}
\value{
  Returns objects of \code{class} vector, factor, data.frame or matrix
  in the same way as \code{\link{na.omit}} does.  Returns all other
  objects unchanged and prints a warning. 
}

\author{ Rene Locher }

\seealso{\code{\link{na.omit}}}

\examples{
M <- matrix(c(NA,1:7,NA),nrow=3)
M
NaRV.omit(M)

DF <- iris[sample(1:nrow(iris),12),]
DF[1,1] <- NA
DF[10,5] <- NA
row.names(DF) <- 1:12
DF
NaRV.omit(DF)

NaRV.omit(c(NA,1:10,NA))

fac <- factor(c(NA,sample(c(1:9))))
NaRV.omit(fac)

fac <- factor(c(NA,sample(c(1:9))),exclude=NULL)
fac
NaRV.omit(fac)
}

\keyword{ manip }

