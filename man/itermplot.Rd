\name{itermplot}
\alias{itermplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plot Regression Terms for Huge Datasets }
\description{
Plots regression terms against their predictors, optionally with
standard errors and partial residuals in a density plot.
}
\usage{
itermplot(model, data = NULL, envir = environment(formula(model)),
          partial.resid = FALSE, scale=0, pixs = 1,
          zmax=NULL, ztransf = function(x) {x}, colramp = IDPcolorRamp,
          terms = NULL, se = FALSE,
          xlabs = NULL, ylabs = NULL, main = NULL,
          col.term = "black", lwd.term = 2,
          col.se = "gray", lty.se = 2, lwd.se = 1,
          col.smth = "darkred", lty.smth = 2,
          lwd.smth = 2, span.smth = 2/3,
          ask = interactive() && nb.fig < n.tms &&
                .Device != "postscript",
          use.factor.levels = TRUE, smooth = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{ Fitted model object }
  \item{data}{ Data frame in which variables in model can be found }
  \item{envir}{ Environment in which variables in model can be found }
  \item{partial.resid}{ Logical; should partial residuals be plotted? }
  \item{scale}{ A lower limit for the number of units covered by the
    limits on the `y' for each plot. The default is \code{scale = 0}, in
    which case each plot uses the range of the functions being plotted
    to create their ylim. By setting scale to be the maximum value of
    diff(ylim) or above for all the plots, then all subsequent plots
    will be produced in the same vertical units. This is essential for
    comparing the importance of fitted terms in additive models. }
  \item{pixs}{ Size of pixel in x- and y-direction in [mm] on the
    plotting device. When x and y are numeric, pixels are square. When x
    and y are factors, pixels are no longer square. The pixels are
    enlarged in x-direction. }
  \item{zmax}{ Maximum number of counts per pixel in the plot.  When
    NULL, each scatter plot has its individual scale.  If a number >=
    maximum number of counts per pixel is supplied, the scale will be
    identical for all scatter plots.  The maximum number of counts per
    pixel is delivered by the return value. }
  \item{ztransf}{ Function to transform the number of counts per pixel.
    The user has to make sure that the transformed density lies in the
    range [0,zmax], where zmax is any positive number (>=2).  For
    examples see \code{\link{ipairs}} and \code{\link{ilagplot}}.}
  \item{colramp}{ Color ramp to encode the number of counts within a
    pixel by color. }
  \item{terms}{ Numeric. Which terms to plot (default NULL means all
    terms) }
  \item{se}{ Logical. Plot pointwise standard errors? }
  \item{xlabs}{ Vector of labels for the x axes }
  \item{ylabs}{ Vector of labels for the y axes }
  \item{main}{ Logical, or vector of main titles; if TRUE, the model's
    call is taken as main title, NULL or FALSE mean no titles. }
  \item{col.term, lwd.term}{ Color and line width for the \dQuote{term
    curve} }
  \item{col.se, lty.se, lwd.se}{ Color, line type and line width for the
    \dQuote{twice-standard-error curve} when se = TRUE. }
  \item{col.smth, lty.smth, lwd.smth}{ Color, line type and line width
    for the smoothed curve}
  \item{span.smth}{ Smoothing parameter f for \code{\link[stats]{lowess}}.}
  \item{ask}{ Logical. Should user be asked before each plot? cf.
    \code{\link{par}}. }
  \item{use.factor.levels}{ Logical. Should x-axis ticks use factor
    levels or numbers for factor terms? }
  \item{smooth}{ NULL or a function with the same arguments as
    \code{\link{ipanel.smooth}} to draw a smooth through the partial
    residuals for non-factor terms }
  \item{\dots}{ Other graphical parameters  }
}
\details{
  \code{itermplot} is a modified version of
  \code{\link[stats]{termplot}} of R V2.3.1. Partial residuals are
  displayed here as a density plot and is therfore especially suited for
  models of huge datasets.
  The model object must have a predict method that accepts type=terms,
  eg glm in the base package, coxph and survreg in the survival
  package.

  For the partial.resid=TRUE option it must have a residuals method that
  accepts type="partial", which \code{\link{lm}},  \code{\link{glm}} and
  \code{\link[mgcv]{gam}} do.

  The data argument should rarely be needed, but in some cases termplot
  may be unable to reconstruct the original data frame. Using
  na.action=na.exclude makes these problems less likely.

  Nothing sensible happens for interaction terms.
}
\value{
  Maximum number of counts per pixel found.
}
%%\references{  }
\author{ Rene Locher }

\seealso{ \code{\link{termplot}}. }
\examples{
r.lm <- lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width+Species,
           data=iris)
par(mfrow=c(2,2),pty="s")
itermplot(r.lm, se = TRUE, partial.res=TRUE, lwd.term = 3,
          lwd.se = 2, pixs = 2)

if (require(SwissAir)) {
  data(AirQual)
  r.lm <- lm(log(ad.O3)~log(ad.NOx)+ad.T+ad.Td+ad.WS, data=AirQual)
  par(mfrow=c(2,2),pty="s")
  itermplot(r.lm, se = TRUE, partial.resid=TRUE, smooth=ipanel.smooth,
            lwd.smth = 3, pixs = 1, ask=FALSE)
} else print("Package SwissAir is not available")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }

