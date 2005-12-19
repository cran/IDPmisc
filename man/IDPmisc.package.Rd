\name{IDPmisc-package}
\alias{IDPmisc}
\docType{package}

\title{Utilities of Institute of Data Analyses and Process Design (www.idp.zhwin.ch)
}

\description{
  The IDPmisc package contains different high-level graphics functions
  for displaying large datasets, displaying circular data in a very
  flexible way, brewing color ramps, drawing nice arrows, zooming
  2D-plots, creating figures with differently colored margins and plot
  regions. In addition, the package contains auxiliary functions for
  data manipulation like omitting observations with irregular values or
  selecting data by logical vectors, which include NAs. 
}

\details{
\tabular{ll}{
  Package: \tab IDPmisc\cr
  Type: \tab Package\cr
  Version: \tab 0.5.7\cr
  Date: \tab 2005-12-16\cr
  License: \tab  GPL (GNU Public Licence), Version 2 or later\cr
}

\tabular{ll}{
  Function \tab                              Description\cr
  \code{\link{Arrows}} \tab                  Pretty Open or Closed Arrows\cr
  \code{\link{cart2clock}} \tab              Converts Cartesian Coordinates to Clock Coordinates\cr
  \code{\link{clock2cart}} \tab              Converts Clock Coordinates to Cartesian Coordinates\cr
  \code{\link{col2hsv}} \tab                 Converting Color to hsv Code\cr
  \code{\link{data.sheet}} \tab              Coercing a list to a data.frame\cr
  \code{\link{IDPcolorRamp}} \tab            Color Ramp for Ordered Values\cr
  \code{\link{ilagplot}} \tab                Image Lag Plot Matrix for Large Time Series\cr
  \code{\link{Image}} \tab                   Displays the Density of Points in a Scatter Plot by Colors\cr
  \code{\link{ipairs}} \tab                  Image Scatter Plot Matrix for Large Datasets\cr
  \code{\link{ixyplot}} \tab                 Image Scatter Plot for Large Datasets\cr
  \code{\link{NaRV.omit}} \tab               Omits Observations with NA, NaN, Inf and -Inf Values\cr
  \code{\link{ok}} \tab                      Sets NAs in Logical Objects to FALSE\cr
  \code{\link{plot}} \tab                    Plots objects of Class Rose\cr
  \code{\link{poster.plot}} \tab             Convenient xyplot with Differently Colored Margin and Plot Region\cr
  \code{\link{showColors}} \tab              Displays vectors of colors\cr
  \code{\link{zoom}}  \tab                   Zooming in and out in a 2d-Plot\cr
}

}

\author{
Rene Locher, Andreas Ruckstuhl, Thomas Unternährer

Maintainer: Rene Locher <rene.locher@zhwin.ch>
}

%% references{
%% ~ Literature or other references for background information ~~
%%

\keyword{ package }
\keyword{ dplot }
\keyword{ hplot }
\keyword{ manip }
\keyword{ utilities }

%% \seealso{
%% ~~ Optional links to other man pages, e.g. ~~
%% ~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%% }
%% 
%% \examples{
%% ~~ simple examples of the most important functions ~~
%% }
