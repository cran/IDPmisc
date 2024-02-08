\name{MS}
\alias{MS}
\docType{data}

\title{ Spectrum Measured by a SELDI TOF Mass Spectrometer}

\description{
  The spectrum was taken from a sample of sheep blood. The instrument
  used was a so called SELDI TOF (Surface Enhanced
  Laser Desorption Ionisation, Time Of Flight) Mass Spectrometer.
}

\usage{data(MS)}
\format{
  A data frame with 45213 observations on the following 2 variables.
  \describe{
    \item{\code{mz}}{mass / charge}
    \item{\code{I}}{Intensity}
  }
}

\details{
   The measured masses lie between m/z=1000 and 200000. The intensities
   are raw output. Neither smoothing nor background subtraction was
   applied to the spectrum.
 }

\source{
  Medical research project.
}

%% \references{ }

\examples{
data(MS)

MS1 <- log10(MS[MS$mz>12000&MS$mz<1e5,])
P <- peaks(MS1, minPH=0.025, minPW=0.0015)

plot(MS1, ty="l", xlim=c(4.15,5))
points(P,col="red")

}

\keyword{datasets}
