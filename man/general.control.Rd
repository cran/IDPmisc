\name{general.control}
\alias{general.control}


\title{ Auxilary for Controlling  the General Appearance of a Rose Plot
}

\description{
  Defines  the General Appearance of rose plot.
}

\usage{
general.control(stacked = FALSE,
                rose.rad = NULL, rose.x = NULL, rose.y = NULL,
                mar = rep(0.3, 4),
                rev.col = FALSE,
                shift = 0,
                cex = 1, col = NULL, lty = 1:3, lwd = 1, type = "s")
}

\arguments{
  \item{stacked}{ \code{stacked = FALSE}: For each point
    \code{(object@rho[i,j], object@cyclVar[i])} with \code{x} equal to a
    \code{\link{rose}} object, the radius is \code{object@rho[i,j]} -
      \code{ray.lim[1]} as defined in \code{\link{grid.control}}.  Points
      with identical \code{j} are connected by a colored line.
      \cr \code{stacked = TRUE}: For each point
      \code{(object@rho[i,j], object@cyclVar[i])},
      the radius is \code{sum(object@rho[1:i,j])}.  No negative values are
      allowed in the stacked case as this feature makes sense only for
      variables like proportions, counts or concentrations.  Areas
      between two adjacent \code{j} are filled by color.
      \cr \code{NA} values in \code{object@rho} are interpreted as \code{0}
      and a warning is issued.}
  \item{rose.rad, rose.x, rose.y}{ Approximate length of radius, x- and
    y-position of rose.  Default units are mm but any valid grid unit
    might be chosen (cf. \code{\link[grid]{unit}}.  If one or more of
    these arguments are defined, the user of the plot function must make
    sure that the size of the viewport is large enough to show the
    complete rose and the legend.  These arguments are especially useful
    when a series of plots of exactly the same size and position of the
    rose on the viewport has to be produced.}
  \item{mar}{ Margin around the plotting area. Default units are
  \code{grid.control(cyclVar.cex)}. Other units can be defined by
  \code{\link[grid]{unit}}.}
  \item{rev.col}{ \code{= TRUE}: Ordering of the columns of \code{object@rho}
    is reversed. }
  \item{shift}{ Shifts the plot in clockwise direction by this
    angle.  Units of \code{shift} must be identical with units of
    \code{cyclVar} in \code{link{plot.rose}}}
  \item{cex}{ Basic size of characters in the plot. }
  \item{col}{ Colors of lines when \code{stacked = FALSE} or colors of
    stacked areas else.  In the first case colors are by default as
  distinct as possible; in the latter case, all colors are matched by
  default to the range green (=center of rose) to blue (outside of rose). }
  \item{lty, lwd}{ Line type and line width. When \code{stacked = TRUE}
  and \code{lwd = 0} the colored aereas are drawn without black
  borders.} 
  \item{type}{1-character string giving the type of plot desired: \cr
    "s" plots observations as segments. This option is the proper way to
  distplay rose data. \cr
    "l" plots a line joining the data points. This option is a nicer to
  look at when data are smoothly distributed in all directions.}
}

\value{
  Returns the arguments conveniently packaged up in a list to supply the
  general arguments for \code{\link{plot.rose}}.
}


\author{ Rene Locher }

\seealso{ \code{\link{plot.rose}}, \code{\link{grid.control}}}

\examples{
general.control()
}

\keyword{ dplot }

