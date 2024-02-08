\name{rfbaseline}
\alias{rfbaseline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Robust Fitting of Baselines }
\description{
 Robust fitting of local regression models for estimating a baseline or a background signal
}
\usage{
rfbaseline(x, y, span = 2/3, NoXP = NULL,
           maxit = c(2, 2), b = 3.5, weight = NULL,
           Scale = function(r) median(abs(r))/0.6745,
           delta = NULL, SORT = TRUE, DOT = FALSE, init = NULL)
}

\arguments{
  \item{x, y}{ Abscissa and ordinate of the points on the scatterplot. }
  \item{span}{ Specifies the amount of smoothing; span is the fraction
    of points used to compute each fitted value; as span increases the
    output becomes smoother. }
  \item{NoXP}{ Another way of specifying the amount of smoothing; NoXP
    is the Number of X Points used to compute each fitted value; it must
    be larger than 3.}
  \item{maxit}{ The number of iterations in the robust fit; if
    maxit=c(0,0), the nonrobust fit is  returned; the first entry
    specifies the number of iterations using an asymmetric biweight
    function, whereas the second entry specifies the number of
    iterations using the usual (symmetric) biweight function. }
  \item{b}{ Tuning constant in the biweight function. }
  \item{weight}{ Optional weights to be given to individual
    observations. }
  \item{Scale}{ function specifying how to calculate the scale of the
    residuals. }
  \item{delta}{ Nonnegative parameter which may be used to save
    computation. By default, if \code{length(x) <= 100}, delta is
    set equal to 0; if \code{length(x) > 100} set to 1/100th of the
    range of \code{x}.}
  \item{SORT}{ Boolean variable indicating whether x data must be
    sorted. Change it only when the x are sorted and you want to safe
    computer time. }
  \item{DOT}{ If TRUE disregard outliers totally; that is, observations
    with weight 0 are disregarded even when the neighbourhood is
    determined. }
  \item{init}{ Values of an initial fit. }
}


\value{
  List containing components
  \item{x }{Sorted input vector x with  duplicate points removed}
  \item{y }{Corresponding input vector y}
  \item{fit }{Fitted values at x}
  \item{rw }{Robust weights of (x,y)-Points used in last iteration of fit}
  \item{scale }{Scale used in last iteration of fit}
}
\references{
  Ruckstuhl, Andreas F., Matthew P. Jacobson,   Robert W. Field and James
  A. Dodd (2001); Baseline Subtraction Using Robust Local Regression
  Estimation; Journal of Quantitative Spectroscopy and Radiative
  Transfer \bold{68}: 179 -- 193

  Ruckstuhl, Andreas F., et al.; Estimation of background concentrations
  of atmospheric trace gases using robust local regression; to be
  published }
\author{ Andreas Ruckstuhl }


\seealso{ See Also as \code{\link{loess}} and \code{\link{lowess}} }
\examples{
data(MS)
MS1 <- log10(MS[MS$mz>12000&MS$mz<1e5,])

MS1.rfb2 <- rfbaseline(x=MS1$mz, y=MS1$I, NoXP=2200, maxit=c(5,0))
plot(x=MS1$mz, y=MS1$I, type="l",
     xlab="log(mass/charge)", ylab="log(intensity)")
lines(MS1.rfb2$x, MS1.rfb2$fit, col="orange", lwd=3)

MS1.rfb3 <- rfbaseline(x=MS1$mz, y=MS1$I, NoXP=1100, maxit=c(5,0),
                       DOT=TRUE, Scale=function(x) mad(x, center=0))
plot(x=MS1$mz, y=MS1$I, type="l",
     xlab="log(mass/charge)", ylab="log(intensity)")
lines(MS1.rfb3$x, MS1.rfb3$fit, col="orange", lwd=3)

## 'delta=0' needs much more computer time
\dontrun{
MS1.rfb4 <- rfbaseline(x=MS1$mz, y=MS1$I, NoXP=2200,
                       delta=0, maxit=c(5,0))
plot(x=MS1$mz, y=MS1$I,ty="l",
     xlab="log(mass/charge)", ylab="log(intensity)")
lines(MS1.rfb4$x, MS1.rfb4$fit, col="orange", lwd=3)
}
}

\keyword{ robust }
\keyword{ regression }
\keyword{ smooth }
