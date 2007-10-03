\name{longts.plot}
\alias{longts.plot}

\title{ Plot Very Long Regular Time Series }
\description{
  Plot one or more regular time series in multiple figures on one or
  more pages.
}
\usage{
longts.plot(y1, y2 = NULL,
            names1 = NULL, names2 = NULL,
            startP = start(y1)[1], upf = 14, fpp = 5, overlap = 0.5,
            x.ann = NULL, dx.ann = 1, dx.tick = 0.25 * dx.ann,
            ny.ann = 3, cex.ann = par("cex.axis"),
            xlab = "", y1lab = "", y2lab = "",
            col.y1 = "black", col.y2 = "black",
            cex.lab = par("cex.lab"),
            y1lim = range(y1, na.rm = T, finite=TRUE),
            y2lim = range(y2, na.rm = T, finite=TRUE),
            lty1 = 1, lty2 = 2, lwd1 = 1, lwd2 = 2,
            col1 = NULL, col2 = NULL,
            leg = TRUE, y1nam.leg = NULL, y2nam.leg = NULL,
            ncol.leg = NULL, cex.leg = par("cex"),
            h1 = NULL, h2 = NULL, col.h1 = "gray70", col.h2 = "gray70",
            main = NULL, cex.main = par("cex.main"),
            automain = is.null(main),
            mgp = c(2, 0.7, 0), mar = c(2,3,1,3)+.1,
            oma = if (automain|!is.null(main))
                              c(0,0,2,0) else par("oma"),
            cex = par("cex"), type = "s", slide = FALSE, each.fig = 1,
            filename = NULL, extension = NULL, filetype = NULL, ...)
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
  \item{x.ann}{ Time annotations (character vector) for \emph{each}
    point of \code{y1}, from which algorithm takes the annotations for
    the x-axis. } 
  \item{dx.ann}{ Distance between two annotions on the x-axis in time
    units. } 
  \item{dx.tick}{ Distance between two ticks on the x-axis in time
    units. }
  \item{ny.ann}{ Approximate number of annotations on y-axis  }
  \item{cex.ann}{ The magnification to be used for axis annotation
    relative to the current setting of cex. } 
  \item{xlab}{ Label for x-axis. }
  \item{y1lab, y2lab}{ Labels for y-axes on the left and on the right
    end of the figure. }
  \item{col.y1, col.y2}{ Colors for left and right axis respectively. }
  \item{cex.lab}{ cex of labels. }
  \item{y1lim, y2lim}{ Limits for left and right axis respectively. }
  \item{lty1, lty2}{ Vector of line types for each time series, possibly
    recycled. }
  \item{lwd1, lwd2}{ Vector of line widths for each time series,
    possibly recycled. } 
  \item{col1, col2}{ Vector of color for each time series, possibly
    recycled. } 
  \item{leg}{ Logical. When TRUE, legend is drawn. }
  \item{y1nam.leg, y2nam.leg}{ Name of y1- and y2-axis in legend.  }
  \item{ncol.leg}{ Number of columns in legend. }
  \item{cex.leg}{ cex for legend. }
  \item{h1, h2}{ Vector of y-positions of horizontal lines in the
    coordinate systems represented by the left and the right axis
    respectively. } 
  \item{col.h1, col.h2}{ Vector of colors for horizontal lines. }
  \item{main}{ Title of plot.}
  \item{cex.main}{ cex of title.  }
  \item{automain}{ Logical. When \code{TRUE}, the units of the start and
    the end on the page are printed in the title.}
  \item{mgp, mar, oma, cex}{ Cf. \code{\link[graphics]{par}}. }
  \item{type}{ Cf. \code{\link[graphics]{plot}}. } 
  \item{slide}{ Logical. When \code{FALSE}, the first page (screen) is
    plotted only. When \code{TRUE}, user may leaf through the pages
    interactively. }
  \item{each.fig}{ For \code{each.fig==1} all data are plotted.
                   for \code{each.fig==2} every 2nd page is plotted and
		   so on. }
  \item{filename, extension}{When \code{filename} is not 
    \code{NULL}, the displayed screen(s) are saved to the
    disk. When \code{slide = TRUE}, the filename of the plots is
    extended by a serial number. The extension is defined automatically
    on windows systems and must be defined manually on the other
    systems.} 
  \item{filetype, ...}{\emph{On windows operating systems}, these
    arguments are identical with the arguments in
    \code{\link[grDevices]{savePlot}(filename = filename, type =
      filetype, ...)}, where filename ist defined above. The filetype
    defaults to windows meta file, \code{\dQuote{wmf}} , with the
    extension \code{\dQuote{.wmf}}.\cr 
  \emph{On non windows operating systems}, these
    arguments are identical with the arguments in
    \code{\link[grDevices]{devPrint}(filename = filename, device =
      filetype, ...)}, , where filename ist defined above. The filetype
    defaults to \code{postscript} , with the extension
    \code{\dQuote{.ps}}.}  
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
  graphic device and, optionally, saved in one or more files.
}

%%\references{ ~put references to the literature/web site here ~ }

\author{ Rene Locher }

\note{Labeling of x-axis is only done within the time range of
  \code{y1}
}

%%\seealso{ }

\examples{
data(sunspots)
longts.plot(sunspots,upf=ceiling((end(sunspots)-start(sunspots))[1]/5),
            dx.ann=10, dx.tick=1)

if (require(SwissAir)) {
  data(AirQual)

  longts.plot(y1=AirQual[,c("ad.O3","ad.NOx")], y2 = AirQual$ad.T,
              names1=c("O3","NOx"),names2="Temp",
              startP = 6.5*30*48, upf=7*48, dx.ann=48,
              x.ann=substr(AirQual$start,1,6),
              y1lab="[ppb]", y2lab="[°C]",
              y1lim=c(0,150), y2lim=c(10,30),
              col2="red", type="l")
}

}

\keyword{ hplot }
\keyword{ iplot }
\keyword{ ts }
\keyword{ multivariate }
