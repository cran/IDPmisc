\name{draw.leg}
\alias{draw.leg}

\title{Produce a Legend or Key (Grid Function)}
\description{
  Produces (and possibly draws) a Grid frame grob which is a legend that
  can be placed in other Grid plots.  \code{draw.leg} is a
  slightly enhanced version of \code{\link[lattice]{draw.key}} in
  package \pkg{lattice} V 0.12-3.
}
\usage{
draw.leg(key, draw=FALSE, vp=NULL)
}
\arguments{
  \item{key}{ A list determining the key.  See details below and the
    documentation for \code{\link[lattice]{xyplot}}.
  }
  \item{draw}{ logical, whether the grob is to be drawn. }
  \item{vp}{ viewport }
}
\value{
  A Grid frame object (that inherits from ``grob'')
}

\details{
  Three new components are added to the list \code{key} of the original code
  in package \pkg{lattice} V 0.12-3: \code{between.rows},
  \code{between.title}, \code{adj.title}.  They allow to format the
  legend in a more flexible way.  To ease the use of \code{draw.leg}, the
  full description of \code{\link[lattice]{draw.key}} and the here
  interesting part of \code{\link[lattice]{xyplot}} are also included:
  \cr
  The key essentially consists of a number of columns, possibly
  divided into blocks, each containing some rows.   The contents of the
  key are determined by (possibly repeated) components named
  ``rectangles'', ``lines'', ``points'' or ``text''.  Each of these
  must be lists with relevant graphical parameters (see later)
  controlling their appearance.  The \code{key} list itself can contain
  graphical parameters, these would be used if relevant graphical
  components are omitted from the other components.

  The length (number of rows) of each such column (except ``text''s)
  is taken to be the largest of the lengths of the graphical
  components, including the ones specified outside (see the entry for
  \code{rep} below for details on this).  The ``text'' component has to
  have a character or expression vector as its first component, and
  the length of this vector determines the number of rows.

  The graphical components that can be included in \code{key} (and
  also in the components named ``text'', ``lines'', ``points'' and
  ``rectangles'' as appropriate) are:

  \itemize{
    \item \code{cex=1}
    \item \code{col="black"}
    \item \code{lty=1}
    \item \code{lwd=1}
    \item \code{font=1}
    \item \code{fontface}
    \item \code{fontfamily}
    \item \code{pch=8}
    \item \code{adj=0}
    \item \code{type="l"}
    \item \code{size=5}
    \item \code{angle=0}
    \item \code{density=-1}
  }

  \code{adj}, \code{angle} and \code{density} are currently
  unimplemented.   \code{size} determines the width of columns of
  rectangles and lines in character widths.  \code{type} is relevant
  for lines; \code{"l"} denotes a line, \code{"p"} denotes a point,
  and \code{"b"} and \code{"o"} both denote both together.

  Other possible components of \code{key} are:

  \describe{
    \item{\code{between}}{
      numeric vector giving the amount of space (character widths)
      surrounding each column (split equally on both sides),
    }
    \item{\code{title}}{
      string or expression giving a title for the key
    }
    \item{\code{rep}}{
      logical, defaults to \code{TRUE}.   By default, it's assumed that
      all columns in the key (except the ``text''s) will have the same
      number of rows, and all components are replicated to be as long
      as the longest.  This can be suppressed by specifying
      \code{rep=FALSE}, in which case the length of each column will
      be determined by components of that column alone.
    }
    \item{\code{cex.title}}{
      cex for the title
    }
    \item{\code{background}}{
      background color, defaults to default background
    }
    \item{\code{border}}{
      either a color for the border, or a logical.  In the latter
      case, the border color is black if \code{border} is
      \code{TRUE}, and no border is drawn if it is \code{FALSE} (the
      default)
    }
    \item{\code{transparent=FALSE}}{
      logical, whether key area should have a transparent background
    }
    \item{\code{columns}}{
      the number of columns column-blocks the key is to be divided
      into, which are drawn side by side.
    }
    \item{\code{between.columns}}{
      Space between column blocks, in addition to \code{between}.
    }
     \item{\code{between.rows}}{
      New argument: Space between rows.
    }
     \item{\code{between.title}}{
      New argument: Space between top row and title of legend.
    }
     \item{\code{adj.title}}{
      New argument: Adjustment of title in respect to body of
      legend.
      \cr \code{adj.title = 0} adjusts the title to the left,
      \cr \code{adj.title = 1} adjusts the title to the right and
      \cr \code{adj.title = 0.5} (=default) centers the title.
    }
    \item{\code{divide}}{
      Number of point symbols to divide each line when \code{type} is
      \code{"b"} or \code{"o"} in \code{lines}.
    }
  }

}

\examples{
require(grid)

## Legend 2 cm below and 1 cm left of the upper right corner
grid.newpage()
key <- draw.leg(key =
                    list(rectangles = list(col=1:3,
                             size = 2,
                             lwd = 0.5),
                         text = list(LETTERS[1:3]),
                         cex = 1.2,
                         between = 2,
                         between.rows = 0.5,
                         between.title = 0.7,
                         title = "component",
                         cex.title = 1.4,
                         transparent = TRUE))

vp.key <-  viewport(x = convertX(unit(1,"npc")-unit(1,"cm"),"cm"),
                    y = convertY(unit(1,"npc")-unit(2,"cm"),"cm"),
                    width=grobWidth(key),
                    height=grobHeight(key),
                    just=c("right","top"))

pushViewport(vp.key)
grid.draw(key)
popViewport()

##----------------------------------------
## Legend 1 cm above the lower left corner
key <- draw.leg(key = list(rectangles = list(col=1:3,
                             size = 4,
                             lwd = 0.5),
                  text=list(letters[1:3]),
                  lines=list(col=1:3),
                  cex=2,
                  between=2,
                  between.rows=0.5,
                  between.title=0.7,
                  title = "component",
                  adj.title = 0,
                  cex.title = 2.4,
                  transparent = TRUE))

vp.key <-  viewport(x = 0,
                    y = unit(1,"cm"),
                    width=grobWidth(key),
                    height=grobHeight(key),
                    just=c("left","bottom"))

grid.newpage()
pushViewport(vp.key)
grid.draw(key)
popViewport()
}
\author{Deepayan Sarkar, modified by Rene Locher}
\seealso{\code{\link[lattice]{xyplot}}}
\keyword{dplot}

