\name{poster.plot}
\alias{poster.plot}

\title{Convenient xyplot with Differently Colored Margin and Plot Region}
\description{
  Convenient xyplot with Colored Background.  Background of margin may be
  chosen independently from background in plot region.
}

\usage{
poster.plot(x, y = NULL, type = "p",
            col = col.fg, col.axis = col.fg, col.lab = col.fg,
            col.fg = "blue", col.bg = "lavender", col.box = "cornsilk",
            xlim = NULL, ylim = NULL, xlab = "", ylab = "",
            main = "", cex = 1.2, axes = TRUE, ...)
}

\arguments{
  \item{x}{A vector, data.frame or matrix.  When x is data.frame or
    matrix only first two columns are used.}
  \item{y}{A vector or \code{NULL}}
  \item{type}{See Argument \code{type} in \code{\link{par}}}
  \item{col}{Color of points.  If \code{length(col)} > 1, colors are recycled.}
  \item{col.axis}{Color of axis.}
  \item{col.lab}{Color  of labels on axis.}
  \item{col.fg}{Color  of foreground.}
  \item{col.bg}{Color of background outside of figure.}
  \item{col.box}{Color of background inside of figure}
  \item{xlim}{Limits of x-axis.}
  \item{ylim}{Limits of y-axis}
  \item{xlab}{Label of x-axis}
  \item{ylab}{Label of y-axis}
  \item{main}{Titel.}
  \item{cex}{Size of characters.}
  \item{axes}{Should axis be plotted?}
  \item{\dots}{Additional arguments to \code{\link{par}}.}
}

\details{
Side effect: \code{par} options will remain changed so that other
graphic elements can be added comfortably.
  }

\author{Andreas Ruckstuhl, refined by Rene Locher}

\examples{
poster.plot(iris[,1],iris[,2],
            xlab="Sepal.Length", ylab="Sepal.Width")

poster.plot(iris[,1], col="red", col.box="grey95", ylab="Sepal.Length")

## plotting lines
n <- 200
freq <- 1:n
y <- sin(freq/n*4*pi)*cos(freq/n*3*pi) + 0.1*rnorm(n)
poster.plot(y,col.fg="grey30",type="l")
}

\keyword{hplot}
