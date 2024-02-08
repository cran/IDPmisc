\name{getXY}
\alias{getXY}

\title{ Easy and Flexible Input for One- and Two Dimensional Data }

\description{
  The function accepts one- or two dimensional data, checks them for
  compatibility and gives a dataframe back.
}

\usage{
getXY(x, y = NULL, unidim.allowed = TRUE)
}

\arguments{
  \item{x}{ Vector, matrix, dataframe or list. If \code{x} is a vector,
    the second dimension may be defined by argument \code{y}. If
    \code{x} is a matrix, dataframe or list and a second column or
    element exists, this second element is used instead of \code{y}. }
  \item{y}{ Optional vector of the same length as \code{x}.
    \cr When argument \code{x} is onedimensional, argument \code{y} does
    not exist \emph{and} unidim.allowed is TRUE, argument \code{x} is
    coerced to a vector and returned as \code{y} component where the
    resulting \code{x} is just the index vector 1:n. }
  \item{unidim.allowed}{ Logical.
    \cr When unidim.allowed is TRUE (=default), one and two dimensional
    input is accepted for any reasonable combination of x and y. In this
    mode \code{getXY} behaves very similar to \code{\link{xy.coords}}
    \cr An error message is returned, when \code{unidim.allowed} is
    FALSE, argument \code{x} is onedimenional und \code{y} does not
    exist.}
}

\details{
  Input is checked for compatibility: When \code{x} is a list, the
  first two elements must have identical length. When \code{y} is
  defined, \code{x} must be one dimensional and must have the same
  length as argument \code{y}. When \code{onedim.allowed} is
  FALSE, input must be twodimensional.

  When input is one dimensional and \code{unidim} is TRUE, \code{y}
  gives the numbers of elements back.

  Function works similar to \code{\link[grDevices]{xy.coords}}
}

\value{
  dataframe with the components \code{x} and \code{y}
}

\author{ Rene Locher }

\examples{
getXY(3:4, 1:2)
getXY(matrix(1:4,ncol=2))
getXY(as.data.frame(matrix(1:4,ncol=2)))
getXY(4:1)
getXY(list(a=1:2,b=9:10))
}

\keyword{ manip }
\keyword{ error }
