\name{ixyplot}
\alias{ixyplot}
\concept{large dataset}
\concept{huge dataset}

\title{Image Scatter Plot of Large 2d Datasets}
\description{
  Produces an image scatter plot of large 2d datasets where the colors
  encode the density of the points in the scatter plot. Works also with factors.
}
\usage{
ixyplot(x, y = NULL,
        pixs = 1, zmax = NULL, colramp = IDPcolorRamp,
        border = FALSE, xlab = NULL, ylab = NULL,
        main = NULL, cex.main = par("cex.main"))
}

\arguments{
  \item{x}{Object of dimension (n,1) or (n,2) when y not present.
    \crVector of length(n) when y present.}
  \item{y}{Vector of \code{length(n)} or \code{NULL}.}
  \item{pixs}{Pixelsize in mm.}
  \item{zmax}{Maximum counts per pixel to be plotted. \code{zmax} must
    be >= maximum number of counts per pixel.}
  \item{colramp}{Color ramp to encode the density of the points within a
    pixel.}
  \item{border}{Shall border be drawn between the colors in the legend:
    \code{TRUE} / \code{FALSE}.}
  \item{xlab}{Labels of x-axis.}
  \item{ylab}{Labels of y-axis.}
  \item{main}{Title.}
  \item{cex.main}{Size of title.}
}
\details{
  The idea is similar to \code{\link[hexbin]{plot.hexbin}}. The hexagons are
  better suited to reflect the density of points in a plane than the
  squares used here. Nevertheless squares are, contrary to hexagons,
  invariant to reflexions at the x- \emph{and} y-axis and therefore suited for
  scatter plot matrices and also for plotting factors.
}
\value{
   Maximum number of counts per Pixel found.
}

\author{Andreas Ruckstuhl, refined by Rene Locher}
\note{When you get the error message "Zmax too small! Densiest aereas
  are out of range!" you must run the function again without specifying
  zmax. The value returned gives you the minimum value allowed for zmax.}

\seealso{\code{\link{ilagplot}}, \code{\link{ipairs}}}

\examples{
x <-  rnorm(100000)
y <- atan(rnorm(100000,0))
ixyplot(x,y)
ixyplot(x,pixs=2)
ixyplot(iris[,1:2],pixs=4)
ixyplot(iris[,"Petal.Width"],iris[,"Species"],pixs=4)
ixyplot(x=iris[,"Species"],y=iris[,"Petal.Width"],pixs=4,border=TRUE,
        xlab="Species",
        ylab="Petal Width",
        main="Iris Species and Petal Width")
}

\keyword{hplot}
