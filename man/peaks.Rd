\name{peaks}
\alias{peaks}

\title{ Finding Peaks in Raw Data }

\description{
  Returns position, signal height and approximate width at half
  maximum peak height. }

\usage{
peaks(x, y = NULL, minPH, minPW, thr, stepF = 0.49)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{ Position and height of signal. Any reasonable way of
    defining the coordinates is acceptable. See function
    \code{link{getXY}} for details.}
  \item{minPH}{ Mimimum height of peak to be reported. }
  \item{minPW}{ Minimum width of peak at half maximum to be reported. }
  \item{thr}{ Threshold below which the signal is not processed. }
  \item{stepF}{ \code{StepF} defines indirectly the accuracy of the
    selection criteria \code{minPH} and \code{minPW} and of the value of
    the calculated width: The smaller the more accurate and the slower
    the function. It must be <0.5}
}

\details{
  The function is especially useful for signals in which both very broad
  and very narrow peaks are of interest. The peaks may lie very close to
  each other or might even be superpositioned on top of each
  other, e.g. peaks on broader shoulders. The algorithm is also very
  useful when the resolution of the signal is poor and the noise is
  small.

  The function is looking for peaks without any preceding baseline
  substraction or smoothing, which could distort the spectrum.

  The selection criteria \code{minPH} and \code{minPW} and the values
  for the calculated peak widths are only approximate.}

\value{
 dataframe consisting of
  \item{x }{Position of peak}
  \item{y }{Signal height}
  \item{w }{Approximate width at half maximum of peak}
}

\author{ Rene Locher }

\note{
  In the function, the main selection criterium for the peaks is the
  height of the peaks, the second optional criterium is the width of the
  peaks.
}

%% \seealso{  }
\examples{
n <- 200
freq <- 1:n
theory <- sin(freq/n*4*pi)*cos(freq/n*3*pi)
spec <- theory + 0.1*rnorm(n)

plot(spec,type="b")
lines(theory,lwd=2)

pts <- peaks(spec, minPH=0.7)
points(pts,col="red",cex=1.2, pch=20)

## peaks after smoothing the spectrum
spec.sm <- loess.smooth(freq, spec, span=0.2,
                        degree = 2, evaluation = 100)
lines(spec.sm$x, spec.sm$y, col="steelblue", lwd=2)
pts <- peaks(spec.sm, minPH=0.4)
points(pts,col="green",cex=1.2,pch=20)

## Analyses of Mass Spectrum between 12000 and 100'000
## without smoothing, without baseline substraction
data(MS)
MS1 <- log10(MS[MS$mz>12000&MS$mz<1e5,])

P <- peaks(MS1, minPH=0.02, minPW=0.001)
plot(MS1, type="l", xlab="log10(mz)", ylab="log10(I)")
points(P,col="blue",cex=1.6)

}

\keyword{ manip }
\keyword{ dplot }
