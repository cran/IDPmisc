\name{rose}
\alias{rose}

\title{Creates a rose object out of circular data}
\description{
  \code{rose} splits data into subsets according to one or two
  grouping elements, computes summary statistics for each, and returns
  the result in a rose object.
}

\usage{
rose(x, subset = NULL,
     cyclVar = NULL, circle = NULL, n.cyclVar = 8,
     cut = NULL, labels = NULL,
     breaks = NULL, include.lowest = FALSE, right = TRUE, dig.lab = 2,
     warn = TRUE, FUN = mean, ...)
}

\arguments{
  \item{x}{Vector, data frame or matrix containing the response.}
  \item{subset}{ An optional vector specifying a subset of observations
    to be used in the aggregating process. }
  \item{cyclVar}{Cyclic variable as first grouping
    element.  \code{cyclVar} must be a numeric vector whose length is
    equal to the number of rows in \code{x} with 0 <= \code{cyclVar} <
    \code{circle}.  Observations where cyclic variables are NA are
    automatically excluded from the rose object. }
  \item{circle}{Defines the value of a full circle with no default.}
  \item{n.cyclVar}{Defines the number of equally spaced intervals of the
  cyclic variable, into which the data are split.  The first interval is
  labeled with 0 and is always centered around 0.}
  \item{cut}{Vector of numerics, logicals  or factors as second grouping
    elements.  Its length is equal to the number of rows in
    \code{x}.  \code{cut} is used  to group the observations similar to
    function \code{\link[base]{cut}}. }
  \item{labels}{ Labels for the corresponding intervals.  When cut is a
    logical, labels has to be named in the order: FALSE, TRUE.}
  \item{breaks, include.lowest, right, dig.lab}{ These arguments are
    only active when \code{cut} is numeric and  are used
    in exactly the same way as in \link[base]{cut}. \cr
    \code{breaks} defines the break points. \cr
    \code{include.lowest = TRUE} indicates that the lowest (or
    highest, for right = FALSE) breaks value should be also
    included. \cr
    \code{right = TRUE} indicates that the intervals should be closed
    on the right (and open on the left) or vice versa for \code{right =
      FALSE}. \cr
    \code{dig.lab} number of digits for breaks labeling when labels
    are \emph{not} given explicitely.}
  \item{warn}{ Logical, indicating if warnings should be issued
    for NAs in \code{cyclVar} and / or x-values outside of breaks range. }
  \item{FUN}{ Summary function, returning a scalar or vector. }
  \item{\dots}{ Additional arguments for summary function. }
}

\details{
  The first grouping element, \code{cyclVar}, for the summary statistics
  must be circular and numeric.  The second grouping element, \code{cut},
  can be numeric, logical or a factor. \cr
  Not all combinations of arguments are allowed: \cr
  Argument \code{cut} can only be defined when summary consists of a
  scalar and \code{x} consists of 1 column. \cr
  When \code{x} contains only one column and \code{cut} is \emph{not}
  defined, the summary function may also be a vector with the
  restriction, that the summary of each subset, defined by the cyclic
  variable, must have the same number of elements. \cr
  When \code{x} is a data frame or matrix with more than 1 column,
  the summary function must be scalar.
}

\value{
  Object of class \code{rose}
}


\author{ Rene Locher }

\seealso{ \link{rose-class}, \link{plot.rose}, \code{\link{cart2clock}},
 \code{\link{clock2cart}}}

\examples{
## artificial example:
## concentration of A and B as function of
## hour of day (hour) and wind direction (WD)
hour <- rep(0:23,100)
dat <- data.frame(hour = hour,
                  A =    (2*cos((hour+6)/6*pi)+rnorm(24*100,1))^2,
                  B =    (2*cos((hour+4)/6*pi)+rnorm(24*100,1,2))^2,
                  WD =   rnorm(24*100,
                           mean=sample(c(190,220,50),24*100, replace = TRUE),
                           sd=30)\%\%360)

## two different responses, scalar summary function
mean.windrose <- rose(dat[,c("A","B")],
                      cyclVar=dat$WD,
                      circle=360,
                      FUN=mean, na.rm=TRUE)
mean.windrose

## one response, vectorial summary function
quant.dayrose <- rose(dat$A,
                      cyclVar=dat$hour,
                      n.cyclVar=24, circle=24,
                      FUN=quantile, na.rm=TRUE)
quant.dayrose


mean.windroseB <- rose(dat[,c("A")],
                       cyclVar=dat$WD,
                       circle=360,
                       cut=dat$B,
                       breaks=c(0,30,100),
                       dig.lab=3,
                       FUN=mean, na.rm=TRUE)
mean.windroseB

}

\keyword{category}
\keyword{dplot}


