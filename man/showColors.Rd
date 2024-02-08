\name{showColors}
\alias{showColors}

\title{Displays vectors of colors}
\description{
  Displays colors produced by a color vector \code{col} and labels them by the
  corresponding number of the element of \code{col}.
}
\usage{
showColors(col = IDPcolorRamp(20),
           ntm = min(length(col), 20),
           border = TRUE, mar = rep(0,4))
}

\arguments{
  \item{col}{Color vector}
  \item{ntm}{Approximate number of labels printed}
  \item{border}{Shall border be drawn between the colors in the legend:
    \code{TRUE} / \code{FALSE}}
  \item{mar}{Margin. cf \code{\link{par}}}
}

\author{Rene Locher}

\seealso{\code{\link{IDPcolorRamp}},
  \code{\link[RColorBrewer]{ColorBrewer}}}
\examples{
showColors(IDPcolorRamp(5))
showColors(IDPcolorRamp(200),border=FALSE)
showColors(IDPcolorRamp(200),border=FALSE,ntm=5)

showColors(IDPcolorRamp(4,
                        colInt = data.frame(
                            h = c(0.47, 0.28, 0.22, 0.2, 0.00),
                            s = c(0.3, 0.55, 0.75, 0.75, 0.75),
                            v = c(1, 1, 1, 1, 1)),
                        fr     = c(0.2,0.2,0.2))
           ,border=FALSE)

showColors(IDPcolorRamp(200,
                        t(col2hsv(c("darkviolet","blue",
                                    "green","yellow","red"))),
                        fr=rep(0.25,3)),border=FALSE)

}

\keyword{color}
\keyword{hplot}
