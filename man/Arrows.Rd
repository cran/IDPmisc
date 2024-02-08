\name{Arrows}

\alias{Arrows}

\title{Pretty Open or Closed Arrows}

\description{
  Draws a set of open or closed arrows which can be shaped by
  many arguments.  \code{Arrows} is an extended version of
  \code{\link[sfsmisc]{p.arrows}}.
}

\usage{
Arrows(x1, y1, x2, y2, size = 1, width = 1.2/4/cin, open = TRUE,
       sh.adj = 0.1, sh.lwd = 1, sh.col = par("fg"),
       sh.lty = 1,
       h.col = sh.col, h.col.bo = sh.col, h.lwd = sh.lwd, h.lty = sh.lty,
       verbose = FALSE)
}

\arguments{
  \item{x1, y1}{Coordinates of points \bold{from} which to draw.}
  \item{x2, y2}{Coordinates of points \bold{to} which to draw.}
  \item{size}{Head size as a fraction of a character height.}
  \item{width}{Width of the arrow head.  See argument \code{verbose}.}
  \item{open}{Defines if arrows are open or closed.}
  \item{sh.adj}{Defines gap between end of the shaft and the top of the
    head of the arrow (see details)}
  \item{sh.lwd}{Thickness of shaft.  cf. \code{\link{par}}}
  \item{sh.col}{Color of shaft.  cf. \code{\link{par}}}
  \item{sh.lty}{Line type of shaft.  cf. \code{\link{par}}}
  \item{h.col}{Color of head.}
  \item{h.col.bo}{Color of border of head.}
  \item{h.lwd}{Line width of border of head.}
  \item{h.lty}{Line type of border of head.}
  \item{verbose}{When TRUE, the width used is returned.}
}

\details{
  Definition of sh.adj:
  \describe{
    \item{=0}{End of shaft at top of head,}
    \item{=1}{End of shaft at bottom of head,}
    \item{>1}{Gap between shaft and head,}
    \item{<0}{Head is on the shaft.}
    \cr This function is based on \pkg{\link{graphics}}}
}

\value{
A value is only returned, when \code{verbose == TRUE}.
}


\author{
  Andreas Ruckstuhl, refined by Rene Locher
}

\note{
The plotting device should not be resized manually after plotting as
this changes in general the aspect ratio of the plot and deforms hereby
the plotted arrows. The beauty of the arrows depends from the resolution
of the device. The higher resolutions give better results.
}

\seealso{\code{\link[sfsmisc]{p.arrows}}, \code{\link{arrows}}
}

\examples{
## a bunch of different arrows
plot(c(0,10), c(0,10), type="n")
Arrows(5, 5, 5,10, size=3,
       sh.lwd=5, sh.lty=2,
       h.lwd=5)
Arrows(5, 5, 7.5, 9, size=3, open=TRUE,
       sh.adj=0.7, sh.lwd=5, sh.lty=2,
       h.col.bo="red",h.lwd=5)
Arrows(5, 5, 9, 7.5, size=3, open=FALSE,
       sh.adj=1, sh.lwd=5, sh.col="blue",
       h.col.bo="red",h.lwd=2)
Arrows(5, 5, 10, 5, size=2.5, width=1.5, open=FALSE,
       sh.adj=1, sh.lwd=7, sh.col="blue")
Arrows(5, 5, 9, 2.5, size=4, open=FALSE,
       sh.lty=0,
       h.col.bo="black",h.lwd=5)
Arrows(5, 5, 7.5, 1)
Arrows(5, 5, 5, 0, size=2)
Arrows(5, 5, 2.5, 1, size=2, width=1)

## vector field
x<- runif( 20)
y<- runif( 20)
u<- 0.1+0.02*rnorm(20)
v<- 0.1+0.02*rnorm(20)
plot(x,y,xlim=range(c(x,x+u)),ylim=range(c(y,y+v)),type="n")
Arrows(x,y,x+u,y+v,sh.col="blue")
}

\keyword{aplot}

