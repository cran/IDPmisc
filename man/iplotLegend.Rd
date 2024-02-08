\name{iplotLegend}
\alias{iplotLegend}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plots Legend for Color Ramp}
\description{
   Plots legend for color ramp.
}
\usage{
iplotLegend(colramp, ncol = NULL,
            cex.axis = par("cex.axis"), border = FALSE,
            mar = c(0, 0, 0, 3), las = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{colramp}{ Function defining color ramp,
    e.g. \code{\link{IDPcolorRamp}} }
  \item{ncol}{ Number of individual colors in color ramp. See details. }
  \item{cex.axis}{ Magnification to be used for labels. }
  \item{border}{ Logical. When \code{TRUE}, a border is drawn around
    the individual colors in the legend. }
  \item{mar}{ Margin. See \code{\link{par}}. }
  \item{las}{ Orientation of labels on axis. See \code{\link{par}}. }
  \item{\dots}{ Additional arguments to \code{par} }
}
\details{
  A color ramp of \code{ncol+1} individual colors is plotted, starting
  with the background color (= color 0).
  When \code{ncol = NULL} a color ramp of 101 individual colors is
  plotted. The color at the bottom is labeled by '0', the color at the
  top by 'max'.
}

\author{ Rene Locher }

\examples{
\dontrun{
par(oma = rep(2, 4))
layout(matrix(c(1, 2), ncol = 2),
      width = c(lcm(par("csi")*5*2.54), 1),
      heights = 1)}
iplotLegend(IDPcolorRamp, ncol = 15)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }

