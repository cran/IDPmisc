\name{grid.control}
\alias{grid.control}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Auxilary for Controlling  the Grid Appearance of a Rose Plot }
\description{
  Defines the appearance of the guiding elements of rose plots such as
  circles, rays and labels.
}
\usage{
grid.control(circ.n = 4, circ.r = NULL,
             circ.col = "gray30", circ.lwd = 0.5,
             circ.cex = 0.8, circ.between = 0.3,
             circ.dir = pi/16 * 9,
             circ.sub.n = NULL, circ.sub.r = NULL,
             circ.sub.col = "gray70", circ.sub.lwd = 0.5,
             cyclVar.lab = c("N","NE","E","SE","S","SW","W","NW"),
             cyclVar.cex = 1.2, cyclVar.between = 0,
             cyclVar.centered = TRUE,
             ray.lim = NULL, ray.n = 8)
}

\arguments{
  \item{circ.n, circ.r}{ Number of (main)circles or, alternatively,
    radius of circles to be drawn, which will be labeled.}
  \item{circ.col, circ.lwd}{ Color and line width of circles. }
  \item{circ.cex}{ Character size of labels of main circles in
  multiples of \code{cex} as defined in \code{\link{general.control}}. }
  \item{circ.between}{ Distance between labels of the main circle and the
    circle itself in multiples of circ.cex. }
  \item{circ.dir}{ Direction along which the labels of the
    main circles should be drawn, measured clockwise as radian
    from North. }
  \item{circ.sub.n, circ.sub.r}{ Number of subcircle intervals
    between two main circles, or, alternatively, the radii of \emph{all}
    subcircles. }
  \item{circ.sub.col, circ.sub.lwd}{ Color and line width of
    subcircles. } 
  \item{cyclVar.lab}{ Labels of cyclic variable placed along the
    outmost circle. }
  \item{cyclVar.cex}{ Charactersize of labels of cyclic
    variable in multiples of cex as defined in
    \code{\link{general.control}} } 
  \item{cyclVar.between}{ Distance between labels of the cyclic
    variable and the outmost circle of the rose in multiples of
    \code{cyclVar.cex}. }
  \item{cyclVar.centered}{ Labels are positioned relative
    to their center.  \code{cyclVar.centered = FALSE} is used for very
    long labels like (unabbreviated) days of the week.  In this case, the
    labels are positioned relative to the side of the label, which is
    closest to the circle. }
  \item{ray.lim}{ Defines the values for the center and the
    maximum radius in user coordinates.  Be careful with specifying the
    center different from 0 as this might result in misleading roses. }
  \item{ray.n}{ Number of rays. }
}

%% \details{
%% }

\value{
  Returns the arguments conveniently packaged up in a list to supply the
   arguments for the grid appearance of rose plot.
   }
   
\author{ Rene Locher }

%% \note{}

\seealso{ \code{\link{plot.rose}}, \code{\link{general.control}} }
\examples{
grid.control()
}

\keyword{ dplot }
