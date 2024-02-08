\name{zoom}
\alias{zoom}

\title{Zooming in and out in a 2d-Plot}
\description{
  Function to zoom in and out by mouse click in a 2D-plot.
}
\usage{
zoom(fun = plot, zoom.col = "red", delay = 3, ...)
}

\arguments{
  \item{fun}{2D-plotting function}
  \item{zoom.col}{Color of clicked points}
  \item{delay}{Number of sec during which the 2 zooming points are shown
    on the plot before zooming}
  \item{\dots}{Arguments to plotting function}
}

\details{
  When the clicked points lay within the plot region range, the points
  define the new plotting limits.  When the clicked points lay in the
  margin,  the plotting limits will be moved into the corresponding
  direction by 1/3 out of the actual range.  There is no special
  sequential order for the zooming points required.  The zooming function
  is stopped by right clicking and choosing the menu item "stop".
}

\value{
  No value returned.
}

\author{Rene Locher}


\examples{
i <- 1:100
y <- i*sin(i*(pi/16))
y <- c(rev(y),y)

## Not run:
zoom(fun=plot, zoom.col="red", x=1:200, y=y, type="l", xlab="index")
## End( Not run)
}

\keyword{iplot}
\keyword{dynamic}

