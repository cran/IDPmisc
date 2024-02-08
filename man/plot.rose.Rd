\name{plot.rose}
\alias{plot.rose}
\alias{plot}
\alias{plot,rose,missing-method}

\title{ Plot Method for Class "rose"  (Grid Graphics Function)}
\description{
  Describes plot method for class "rose"
}

\usage{
\S4method{plot}{rose,missing}(
          x,
          transf = function(x) sqrt(x),
          subset.col = NULL,
          warn = TRUE,
          general = general.control(),
          grid = grid.control(),
          title = title.control(),
          key = key.control())
}

\arguments{
  \item{x}{ Object \code{\link{rose}}. }
  \item{transf}{ Transformation function for \code{x@rho}.  It defaults to
  the square root, resulting in equal area roses.}
  \item{subset.col}{ Display only a subset of the columns of
    \code{x@rho}.  Subset might be declared by numerical values or by
    name. }
  \item{warn}{ Logical. If \code{FALSE}, warnings are suppressed.}
  \item{general}{ Settings for general appearance of plot, defined in
    \code{\link{general.control}}.}
  \item{grid}{ Settings for appearance of guiding elements of rose such
    as circles, rays and labels, defined in \code{\link{grid.control}}}
  \item{title}{ Settings for title, defined in
    \code{\link{title.control}}.}
  \item{key}{ Settings for appearance of the legend, defined in
    \code{\link{key.control}}.}
}

\details{
  This function appeals especially to environmental specialists who
  often have response variables, which depend from cyclic variables like
  the direction of wind, the hour of the day, the month etc.  All these
  variables are displayed usually clockwise,
  starting with 0 in the north (12 o' clock).  We call this
  kind of coordinates 'clock coordinates', to distinct them from the
  polar coordinates as used in mathematical context.  The
  \code{\link{rose}} object is displayed as the time on a clock,
  measuring the angle defined in slot \code{cyclVar} in the clockwise
  direction from the north. \cr
  The eye takes the area of a graphical object as a measure of its
  size.  This is why the default transformation of \code{x@rho} is
  chosen to be the square root.  For equal distance roses use the
  transformation function \code{function(x) x}. \cr
  All labels, titles and line sizes are defined in multiples of cex. \cr
  This graphic function is based on package \pkg{\link{grid}}:
  Viewport \code{vp.rose} which was used to draw the rose and viewport
  \code{vp.key} which was used to draw the key may be addressed by
  \code{pushviewport()} after having drawn the figure.
}

\value{
  No value returned
}


\author{ Rene Locher }

\note{
The function is designed to use the area on the active viewport in an
optimal way, but the plot is not scalable after having been drawn. \cr
Furthermore this function is still experimental so that some features
may change in future versions. \cr
You will find another nice example for this plot method in \code{\link[SwissAir]{AirQual}}
}

\seealso{ \code{\link{rose}}, \code{\link{rose-class}} }
\examples{
hour <- rep(0:23,100)
WD <- c(rnorm(24*90, mean=sample(c(190,220,50),24*90,
                                          replace = TRUE),sd=10),
                           rnorm(24*10, mean=360, sd=180))\%\%360
dat <- data.frame(A = (2*cos((hour+6)/6*pi)+
                       2*cos((WD+60)/180*pi)+rnorm(24*100,4))^2,
                  B = (2*cos((hour+4)/6*pi)+rnorm(24*100,1,8))^2)
dat$B[dat$B>1000] <- 1000

## two different response variables, scalar summary function
mean.dayrose <- rose(dat[,c("A","B")],
                cyclVar = hour,
                n.cyclVar = 24,
                circle = 24,
                FUN=mean, na.rm=TRUE)

## one response variable, vector summary function
quant.windrose <- rose(dat$A,
                       cyclVar = WD,
                       n.cyclVar = 16, circle = 360,
                       FUN=quantile, na.rm=TRUE)

## one response variable, second (non cyclic) explanatory variable,
## scalar summary function
windrose <- rose(dat[,c("A")],
                 cyclVar = WD,
                 n.cyclVar=8,
                 circle = 360,
                 cut = dat$B,
                 breaks = c(0,10,100,1000),
                 include.lowest = TRUE, dig.lab = 4,
                 FUN = function(x) sum(!is.na(x)))

grid::grid.newpage()
plot(mean.dayrose,
     general = general.control(
       mar = rep(1,4),
       stacked = FALSE,
       lwd = 3,
       lty = c(1:2)),
     grid = grid.control(
       circ.n = 2,
       circ.sub.n = 2,
       circ.lwd = 2,
       circ.sub.col = "black",
       ray.n = 12,
       cyclVar.lab = seq(0,by=2,to=22)),
     title = title.control(text = "unstacked dayrose"),
     key = key.control(title = "Mean",
                       between = 0))

grid::grid.newpage()
plot(quant.windrose)

grid::grid.newpage()
plot(windrose,
     general = general.control(
       stacked = TRUE,
       lwd = 3),
     grid = grid.control(
       circ.n = 2,
       circ.sub.n = 2),
     title = title.control(
       text = "Stacked windrose:\nCounts of A-Values"),
     key = key.control(title = "Value of B"))


if (require(SwissAir)){
  data(AirQual)
  dat <-
    data.frame(month =as.numeric(substr(AirQual$start,4,5)),
               hour = as.numeric(substr(AirQual$start,12,13)),
               WD = AirQual$ad.WD,
               NOx = AirQual$ad.NOx,
               ## NO2 = AirQual$ad.NOx-AirQual$ad.NO,
               ## NO  = AirQual$ad.NO,
               O3  = AirQual$ad.O3,
               Ox  = AirQual$ad.O3+AirQual$ad.NOx-AirQual$ad.NO)


  ## Windrose
  windrose <- rose(dat$WD,
                   cyclVar = dat$WD, n.cyclVar = 32, circle = 360,
                   FUN = function(x) sum(!is.na(x)))

  grid::grid.newpage()
  plot(windrose,
       general =
       general.control(lwd = 2),
       grid =
       grid.control(circ.n = 2,
                    circ.sub.n = 2))

  ## median of concentrations as a function of daytime
  ## from May to September
  med.dayrose <- rose(dat[,c("NOx","O3","Ox")],
                      subset= dat$month>4 & dat$month<10,
                      cyclVar=dat$hour, n.cyclVar=24, circle=24,
                      FUN=median, na.rm=TRUE)

  ## line type version of rose
  grid::grid.newpage()
  plot(med.dayrose,
       general = general.control(lwd=2, type="l"),
       grid =
       grid.control(ray.n = 12,
                    circ.n =2,
                    circ.sub.n = 2,
                    cyclVar.lab = seq(0,by=2,to=22)),
       title = title.control(text =
         "Day Rose of Medians\nduring summer"))

  ## quantiles of concentrations as a function of daytime
  ## from May to September
  quant.dayrose <- rose(dat$NOx,
                        subset= dat$month>4 & dat$month<10,
                        cyclVar=dat$hour, n.cyclVar=24, circle=24,
                        FUN=quantile, na.rm=TRUE)
  grid::grid.newpage()
  plot(quant.dayrose,
       general =
       general.control(mar = c(0.3, 0.3, 0.3, 2),
                       lwd = 2),
       grid =
       grid.control(ray.n = 12,
                    cyclVar.lab = seq(0,by=2,to=22)),
       title = title.control(text = "Concentration of NOx [ppb]\nduring summer"),
       key = key.control(title = "Quantiles"))
} else print("Package SwissAir is not available")

}

\keyword{ hplot }

