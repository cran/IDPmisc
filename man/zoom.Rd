\name{zoom}
\alias{zoom}

\title{Zooming in and out in a 2d-Plot}
\description{
 Function to zoom in and out in a 2D-plot.
}
\usage{
zoom(fun = usa, zoom.col = "red", delay = 3, ...)
}

\arguments{
  \item{fun}{Plotting function}
  \item{zoom.col}{Color of clicked points}
  \item{delay}{Number of sec during which the 2nd zooming point is shown
  before zooming}
  \item{\dots}{Aditional arguments for plotting function}
}

\details{
  When the clicked points lay within the plot region range, the points
  define the new plotting limits. When the clicked points lay in the
  margin,  the plotting limits will be moved into the corresponding
  direction by 1/3 out of the actual range. There is no special
  sequential order for the zooming points required. The zooming function
  is stopped by right clicking and choosing the menu item "stop".
}

\author{Rene Locher}

\examples{
x <- 1:100
y <- x*sin(x*(pi/16))
y <- c(rev(y),y)

if(dev.interactive())
   zoom(fun=plot, ty="l", x=y)
}
\keyword{iplot}
\keyword{dynamic}

