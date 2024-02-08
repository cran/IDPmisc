\name{rose-class}
\alias{rose-class}

\docType{class}

\title{rose-class}

\description{ Summary statistics of cyclic data. }

\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{rose(x, cyclVar = NULL, circle = NULL, n.cyclVar = 8,
    cut = NULL, breaks = NULL,
    labels = NULL, dig.lab = 2,
    include.lowest = FALSE, subset = NULL,
    na.warning = TRUE, FUN = mean, ...) }
}

\section{Slots}{
  \describe{
    \item{\code{rho}:}{Object of class \code{matrix}.  \code{rho[i,]}
      contains the summary values of all data within the interval
      defined by the cyclic Variable \code{cyclVar[i]}.  Column  and row
      names are mandatory.  The different columns of \code{rho}
      correspond to different responses when the summary statistics is a
      scalar \emph{or} to the different elements of a vector summary of
      one response \emph{or} to the different subsets of the second
      grouping element. }
    \item{\code{cyclVar}:}{Object of class \code{numeric} containing
      the center of the interval of the cyclic variable.  The values are
      sorted by increasing values, are unique and cannot contain NA
      values. }
    \item{\code{circle}:}{Scalar of class \code{numeric}, defining the
    full circle. }
  }
}

\author{ Rene Locher }

\seealso{
  For the details of how to create and plot a \code{\link{rose}} object
  see \code{\link{rose}} and \code{\link{plot.rose}}.
}

\keyword{classes}
\keyword{methods}
