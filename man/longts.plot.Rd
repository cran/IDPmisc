\name{longts.plot}
\alias{longts.plot}

\title{ Plots Very Long Regular Time Series }
\description{
  Plots one or more regular time series in multiple figures on one or
  more pages.
}
\usage{
longts.plot(y1, y2 = NULL,
            names1 = NULL, names2 = NULL,
            startP = start(y1)[1], upf = 14, fpp = 5, overlap = 0.5,
            ann.x = NULL, ann.dx = 1, tick.dx = 0.25 * ann.dx,
            ann.ny = 3, xlab = "", y1lab = "", y2lab = "",
            y1.col = "black", y2.col = "black",
            cex.lab = par("cex.lab"),
            y1lim = range(y1, na.rm = T, finite=TRUE),
            y2lim = range(y2, na.rm = T, finite=TRUE),
            lty1 = 1, lty2 = 2, lwd1 = 1, lwd2 = 2,
            col1 = NULL, col2 = NULL,
            leg = TRUE, leg.y1nam = NULL, leg.y2nam = NULL,
            leg.ncol = NULL, leg.cex = 1.5,
            h1 = NULL, h2 = NULL, h1.col = "gray70", h2.col = "gray70",
            main = NULL, main.cex = par("main.cex"), automain = TRUE,
            mgp = c(1.7, 0.7, 0),
            oma = if (automain) c(0, 0, 1, 0) else par("oma"),
            cex = par("cex"), type = "s", slide = FALSE, each.fig = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y1, y2}{ Regular time series, time series matrices or ordinary
    vectors, dataframes or matrices with values corresponding to regular
    time intervals. Corresponding axes are on the left (for \code{y1})
    and on the right (for \code{y2}) respectively. \code{y2} is optional
    and may have a different frequency and a different start time. }
  \item{names1, names2}{ Names for time series used for legend. }
  \item{startP}{ Start time of plot. }
  \item{upf}{ Number of time units plottet per figure. }
  \item{fpp}{ Number of figures per page (screen). }
  \item{overlap}{ Length of time series on the right end of the figures
    which is identical with the left end of the next figure. }
  \item{ann.x}{ Time label (character vector) for \emph{each} point of
    \code{y1}, from which algorithm takes the annotations for the
    x-axis. } 
  \item{ann.dx}{ Distance between two annotions on the x-axis in time
    units. } 
  \item{tick.dx}{ Distance between two ticks on the x-axis in time
    units. }
  \item{ann.ny}{ Time label for \emph{each} point of \emph{\code{y1}},
    from which algorithm takes the annotations for the x-axis. }
  \item{xlab}{ Label for x-axis. }
  \item{y1lab, y2lab}{ Labels for y-axes on the left and on the right
    end of the figure. }
  \item{y1.col, y2.col}{ Colors for left and right axis respectively. }
  \item{cex.lab}{ cex of labels. }
  \item{y1lim, y2lim}{ Limits for left and right axis respectively. }
  \item{lty1, lty2}{ Vector of line types for each time series, possibly
    recycled. }
  \item{lwd1, lwd2}{ Vector of line widths for each time series,
    possibly recycled. } 
  \item{col1, col2}{ Vector of color for each time series, possibly
    recycled. } 
  \item{leg}{ Logical. When TRUE, legend is drawn. }
  \item{leg.y1nam, leg.y2nam}{ Name of y1- and y2-axis in legend.  }
  \item{leg.ncol}{ Number of columns in legend. }
  \item{leg.cex}{ cex for legend. }
  \item{h1, h2}{ Vector of y-positions of horizontal lines in the
    coordinate systems represented by the left and the right axis
    respectively. } 
  \item{h1.col, h2.col}{ Vector of colors for horizontal lines. }
  \item{main}{ Title of plot. }
  \item{main.cex}{ cex of title.  }
  \item{automain}{ Logical. When \code{TRUE}, the units of the start and
    the end on the page are printed in the title. }
  \item{mgp, oma, cex}{ Cf. \code{\link[graphics]{plot}}. }
  \item{type}{ Cf. \code{\link[graphics]{plot}}. } 
  \item{slide}{ Logical. When \code{FALSE}, the first page (screen) is
    plotted only. When \code{TRUE}, user may leaf through the pages
    interactively. }
  \item{each.fig}{ For \code{each.fig==1} all data are plotted.
                   for \code{each.fig==2} every 2nd page is plotted and
		   so on. }
}
\details{
  For longer time-series, it is sometimes important to spread several
  time-series plots over several subplots or even over several pages
  with several subplots in each. Moreover, these series have often
  different ranges, frequencies and start times. There is sometimes also
  the need of a more flexible annotation of the time axis than
  \code{plot.ts} provides. 

  \code{longts.plot} provides the user with all these features for
  regular time series. As in general, time series are often stored 
  together with an eplicit time axis and for the sake of highest
  flexibility, the arguments for controlling the annotation of the time
  axis is not standard: For the annotation, time labels have to be
  delivered for \emph{each} point of \code{y1}, from which algorithm
  takes the annotations for the x-axis.

  \code{\link[sfsmisc]{p.ts}} is another function for plotting long time
  series. The main differences are: \code{p.ts} can also plot irregular
  times series. \code{p.ts} has only one y-axis, cannot automatically
  plot a legend and has the full output always on one page.
}

\section{Side Effects}{
  One or more pages of time series plots are drawn on the current
  graphic device.
}

%%\references{ ~put references to the literature/web site here ~ }

\author{ Rene Locher }

\note{Labeling of x-axis is only done within the time range of
  \code{y1}
}

\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }

\examples{
data(sunspots)
longts.plot(sunspots,upf=ceiling((end(sunspots)-start(sunspots))[1]/5),
            ann.dx=10, tick.dx=1)

if (require(SwissAir)) {
  data(AirQual)

  st <- levels(AirQual$start)[AirQual$start]

  longts.plot(y1=AirQual[,c("ad.O3","ad.NOx")], y2 = AirQual$ad.T,
              names1=c("O3","NOx"),names2="Temp",
              startP = 6.5*30*48, upf=7*48, ann.dx=48,
              ann.x=substr(st,1,6),
              y1lab="[ppb]", y2lab="[°C]",
              y1lim=c(0,150), y2lim=c(10,30),
              col2="red", type="l")
}

}

\keyword{ hplot }
\keyword{ iplot }
\keyword{ ts }
\keyword{ multivariate }
