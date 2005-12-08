"rose.grob" <-
function(pdat,
                      general,
                      grid,
                      title)
  ## Creating the grob rose without legend
  ## No testing is made for any argument!
  ## Author: Rene Locher
  ## Version: 2005-12-06

  {
    rose <-
      gTree(name = "rose",
            children = gList(
              polygonGrob(name = "data",
                          x = pdat$x,
                          y = pdat$y,
                          id = pdat$id,
                          default.units = "native",
                          gp = if (general$stacked) ## colored areas
                               gpar(col = "black", fill = general$col,
                                    lwd = 0.5) else ## colored lines
                               gpar(col = general$col, lwd = general$lwd,
                                    lty = general$lty)
                          ),

              if (grid$circ$sub$plot)
              circleGrob(name = "subcircles",
                         x = 0, y = 0,
                         r = grid$circ$sub$r[grid$circ$sub$r>0],
                         default.units = "native",
                         gp=gpar(col = grid$circ$sub$col,
                           lwd = grid$circ$sub$lwd)),

              circleGrob(name = "circles",
                         x = 0, y = 0,
                         r = grid$circ$r[grid$circ$r>0],
                         default.units = "native", 
                         gp = gpar(col = grid$circ$col,
                           lwd = grid$circ$lwd)),
              
              textGrob(name = "circ.lab",
                       grid$circ$value,
                       x = pdat$circ$lab$x,
                       y = pdat$circ$lab$y,
                       just = c("center","center"),
                       default.units = "native",
                       gp = gpar(cex=grid$circ$cex)),
              
              segmentsGrob(name = "rays",
                           x0 = 0, y0 = 0,
                           x1 = grid$circ$r[length(grid$circ$r)] *
                           sin(2*pi/grid$ray$n*(1:grid$ray$n)),
                           y1 = grid$circ$r[length(grid$circ$r)] *
                           cos(2*pi/grid$ray$n*(1:grid$ray$n)),
                           default.units = "native",
                           gp=gpar(col=grid$circ$col,
                             lwd=grid$circ$lwd)),
              
              textGrob(name = "cyclVar.lab",
                       label = grid$cyclVar$lab,
                       x = pdat$cyclVar$lab$x,
                       y = pdat$cyclVar$lab$y,
                       default.units = "native",
                       just = c("center","center"),
                       gp=gpar(cex = grid$cyclVar$cex)),
              
              if (!is.null(title$text))
              textGrob(name = "title",
                       label = title$text,
                       x = 0,
                       y = pdat$title$y,
                       default.units = "native",
                       just = c("center","bottom"),
                       gp = gpar(cex=title$cex))))
    return(rose)
  } ## rose.grob

