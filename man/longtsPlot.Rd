\name{longtsPlot}
\alias{longtsPlot}

\title{ Plot Very Long Regular Time Series }
\description{
  Plot one or more regular time series in multiple figures on one or
  more pages.
}
\usage{
longtsPlot(y1, y2 = NULL,
           names1 = NULL, names2 = NULL,
           startP = start(y1)[1], upf = 400, fpp = 4, overlap = 20,
           x.at = NULL, x.ann = NULL, x.tick = NULL,
           y1.at = NULL, y1.ann = NULL, y1.tick = NULL,
           y2.at = NULL, y2.ann = NULL, y2.tick = NULL,
           nx.ann = 10, ny.ann = 3, cex.ann = par("cex.axis"),
           xlab = "", y1lab = "", y2lab = "", las = 0,
           col.y1 = "black", col.y2 = col.y1,
           cex.lab = par("cex.lab"),
           y1lim = range(y1, na.rm = TRUE, finite=TRUE),
           y2lim = range(y2, na.rm = TRUE, finite=TRUE),
           lty1 = 1, lty2 = 2, lwd1 = 1, lwd2 = lwd1,
           col1 = NULL, col2 = NULL,
           leg = TRUE, y1nam.leg = NULL, y2nam.leg = NULL,
           ncol.leg = NULL, cex.leg = par("cex"),
           h1 = NULL, h2 = NULL, col.h1 = "gray70", col.h2 = "gray70",
           main = NULL, cex.main = par("cex.main"),
           automain = is.null(main),
           mgp = c(2, 0.7, 0), mar = c(2,3,1,3)+.2,
           oma = if (automain|!is.null(main))
                             c(0,0,2,0) else par("oma"),
           xpd = par("xpd"), cex = par("cex"),
           type1 = "s", type2 = type1,
           pch1 = 46, pch2 = pch1, cex.pt1 = 2, cex.pt2 = cex.pt1,
           slide = FALSE, each.fig = 1,
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
    which is identical with the left start of the next figure. }
  \item{x.at}{ Time points at which long ticks on x-axis and annotations
    are set. }
  \item{x.ann}{ Time annotations (character vector) for long ticks on
    x-axis }
  \item{x.tick}{ Time points at which short ticks are set on x-axis. If
    Null, no short ticks are set. }
  \item{y1.at, y1.ann, y1.tick, y2.at, y2.ann, y2.tick}{Arguments
    starting with 'y1' ('y2') correspond to arguments for left (right)
    axis. For details see arguments for x-axis.}
  \item{nx.ann, ny.ann}{Approximate number of annotations on x- and
    y-axes, respectively }
  \item{cex.ann}{ The magnification to be used for axis annotation
    relative to the current setting of cex. }
  \item{xlab}{ Label for x-axis. }
  \item{y1lab, y2lab}{ Labels for y-axes on the left and on the right
    end of the figure. }
  \item{las}{ Cf. \code{\link[graphics]{par}}. }
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
  \item{mgp, mar, oma}{ Cf. \code{\link[graphics]{par}}. }
  \item{xpd}{ Define whether data points are clipped to the plot region
    (=FALSE) or not (=TRUE). Horizontal lines, defined by \code{h1} or
    \code{h2} are always clipped. }
  \item{cex}{ Cf. \code{\link[graphics]{par}}. }
  \item{type1, type2}{ For plotting lines or
    points. Cf. \code{\link[graphics]{plot}}. }
   \item{pch1, pch2, cex.pt1, cex.pt2}{Plotting symbols and there sizes
     when 'type1' or 'type2' = "p"}
  \item{slide}{ Logical. When \code{FALSE}, the first page (screen) is
    plotted only. When \code{TRUE}, user may leaf through the pages
    interactively. }
  \item{each.fig}{ For \code{each.fig==1} all data are plotted.
                   for \code{each.fig==2} every 2nd page is plotted and
		   so on. }
  \item{filename}{When \code{filename} is not
    \code{NULL}, the displayed screen(s) are saved to
    disk and there is no question whether the next page should be
    printed. When \code{slide = TRUE}, the filename of the plots is
    extended by a serial number.}
   \item{extension}{The extension is defined automatically
    on windows systems and must be defined manually on the other
    systems.}
  \item{filetype}{ \emph{On Windows}:
    The type of plot, Windows metafile, PNG, JPEG,
    BMP (Windows bitmap format), TIFF, PostScript or PDF.
    Defaults to Windows meta file, \code{"wmf"} ,
    with the extension \code{".wmf"}.
    Cf. \code{\link[grDevices]{savePlot}}.
    \cr
    \emph{On non Windows operating systems}: A device
    function (e.g., x11, postscript,...). The filetype
    defaults to \code{postscript} , with the extension
    \code{".ps"}. Cf. \code{\link{dev.print}}
  }
  \item{...}{Additional arguments to
    \code{\link{savePlot}} and
    \code{\link{dev.print}} respectively.
  }
}
\details{
  For longer time-series, it is sometimes important to spread several
  time-series plots over several subplots or even over several pages
  with several subplots in each. Moreover, these series have often
  different ranges, frequencies and start times. There is sometimes also
  the need of a more flexible annotation of axes than \code{plot.ts}
  provides. \code{longtsPlot} provides the user with all these features
  for one or two matrices or regular time series (time series matrices).
}

\section{Side Effects}{
  One or more pages of time series plots are drawn on the current
  graphic device and, optionally, saved in one or more files.
}

%%\references{ ~put references to the literature/web site here ~ }

\author{ Rene Locher }

%%\seealso{ }

\examples{
## sunspots, y-axis only on the left
data(sunspots)
longtsPlot(sunspots,upf=ceiling((end(sunspots)-start(sunspots))[1]/5))

## air quality (left axis) and meteo data (right axis)
## use xpd=TRUE for time series with rare but large values
if (require(SwissAir)) {
  data(AirQual)
  st <- 6.5*30*48
  x.at <- seq(st,nrow(AirQual),48)
  longtsPlot(y1=AirQual[,c("ad.O3","ad.NOx")], y2 = AirQual$ad.T,
             names1=c("O3","NOx"),names2 = "Temp",
             startP = st, upf=7*48,
             x.at = x.at, x.ann = substr(AirQual$start,1,6)[x.at],
             x.tick = seq(st,nrow(AirQual),12),
             y1.at = c(0,100), y1.tick = seq(0,150,50),
             y2.at = c(10,30), y2.tick = seq(10,30,10),
             y1lab="[ppb]", y2lab="[C]",
             y1lim = c(0,100), y2lim = c(10,30), xpd=TRUE,
             col2 = "red", type1 = "l")
}

## Two time series with different frequencies and start times
## on the same figures
set.seed(13)
len <- 4*6*400
x <- sin((1:len)/200*pi)
d <- sin(cumsum(1+ rpois(len, lambda= 2.5)))

y1 <- ts(10*x,start=0,frequency=6)+d*rnorm(len)
y2 <- ts(100*x,start=100,frequency=13)+10*rnorm(len)
longtsPlot(y1,y2)

## plot your own legend
longtsPlot(sunspots, upf = ceiling((end(sunspots)-start(sunspots))[1]/5),
           fpp = 1, leg = FALSE)
legend(1750, 260, legend = "Monthly Sunspot Numbers", col = "blue", lwd = 1,
       bty = "n")
}

\keyword{ hplot }
\keyword{ iplot }
\keyword{ ts }
\keyword{ multivariate }
