`longts.plot` <-
function(y1, y2 = NULL,
                        names1 = NULL, names2 = NULL,
                        startP = start(y1)[1],
                        upf = 14, fpp = 5, overlap = 0.5,
                        x.ann = NULL, dx.ann = 1, dx.tick = 0.25*dx.ann,
                        ny.ann = 3, cex.ann = par("cex.axis"),
                        xlab = "", y1lab = "", y2lab = "",
                        col.y1 = "black", col.y2 = col.y1,
                        cex.lab = par("cex.lab"),
                        y1lim = range(y1,na.rm=T,finite=TRUE),
                        y2lim = range(y2,na.rm=T,finite=TRUE),
                        lty1 = 1, lty2 = 2, lwd1 = 1, lwd2 = lwd1,
                        col1 = NULL, col2 = NULL,
                        leg = TRUE, y1nam.leg = NULL, y2nam.leg = NULL,
                        ncol.leg = NULL, cex.leg = par("cex"),
                        h1 = NULL, h2 = NULL,
                        col.h1 = "gray70", col.h2 = "gray70",
                        main = NULL, cex.main = par("cex.main"),
                        automain = is.null(main),
                        mgp = c(2,0.7,0),
                        mar = c(2,3,1,3)+.1,
                        oma = if (automain|!is.null(main))
                              c(0,0,2,0) else par("oma"),
                        cex = par("cex"),
                        type = "s", slide = FALSE, each.fig = 1,
                        filename = NULL, extension = NULL,
                        filetype = NULL, ...) {
  ## Author:  Rene Locher
  ## Version: 2009-01-29
    .Defunct("longtsPlot", package="IDPmisc")
} #longts.plot

