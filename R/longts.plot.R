`longts.plot` <-
function(y1, y2 = NULL,
                        names1 = NULL, names2 = NULL,
                        startP = start(y1)[1],
                        upf = 14, fpp = 5, overlap = 0.5, 
                        x.ann = NULL, dx.ann = 1, dx.tick = 0.25*dx.ann,
                        ny.ann = 3, cex.ann = par("cex.axis"),
                        xlab = "", y1lab = "", y2lab = "",
                        col.y1 = "black", col.y2 = "black",
                        cex.lab = par("cex.lab"),
                        y1lim = range(y1,na.rm=T,finite=TRUE),
                        y2lim = range(y2,na.rm=T,finite=TRUE),
                        lty1 = 1, lty2 = 2, lwd1 = 1, lwd2 = 2,
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
  ## Version: 2007-02-08

  if (is.null(names1)) {
    names1 <- deparse(substitute(y1))
    if (is.matrix(y1)||is.data.frame(y1)) names1 <- colnames(y1)
    }

  if (is.data.frame(y1)) y1 <- as.matrix(y1)
  if (!is.numeric(y1)) stop("y1 must be numeric!")
  if (!is.ts(y1)) y1 <- ts(y1)
  fq1 <- frequency(y1)

  if (!is.matrix(y1)) {
    y1 <- ts(as.matrix(y1),start=start(y1),freq=fq1)
  }
  
  if (is.null(col1)) col1 <-
    c("blue","green4" ,"red", "darkorchid4", "black", 
      "deepskyblue","green","orange", "violetred", "grey50",
      "saddlebrown")[1:min(nrow(y1),11)]

  len <- ncol(y1)
  lty1 <- rep(lty1,len)[1:len]
  lwd1 <- rep(lwd1,len)[1:len]
  aty1 <- pretty(y1lim,n=ny.ann)

  if (!is.null(x.ann)) {
    if (length(x.ann)!=nrow(y1))
      stop("'x.ann' must have the same length as timeseries 'y1' or must be NULL!", call. = FALSE)
    x.ann <- ts(x.ann,start=start(y1),freq=fq1)
  }  

  if (!is.null(y2)) {
    if (is.null(names2)) {
      names2 <- deparse(substitute(y2))
      if (is.matrix(y2)) names2 <- colnames(y2)
      if (is.data.frame(y2)) {
        names2 <- names(y2)
        y2 <- as.matrix(y2)
      }
    }
    if (!is.numeric(y2)) stop("y2 must be numeric!")
    if (!is.ts(y2)) y2 <- ts(y2)
    fq2 <- frequency(y2)
    
    if (!is.matrix(y2)) {
      y2 <- ts(as.matrix(y2),start=start(y2),freq=fq2)      
    }

    if (ncol(y2)==length(names2)) colnames(y2) <- names2 else
    warning("\nLength of 'names2' is incompatible with dimensions of 'y2'")
    if (is.null(col2)) col2 <-
      c("deepskyblue","green","orange", "violetred","grey50",
        "blue","green4" ,"red", "darkorchid4", "black",
        "saddlebrown")[1:min(nrow(y1),11)]
    
    len <- ncol(y2)
    lty2 <- rep(lty2,len)[1:len]
    lwd2 <- rep(lwd2,len)[1:len]
    
    ## rescaling y2 to y1
    y2 <- (y2-y2lim[1])/diff(range(y2lim))*diff(range(y1lim))+y1lim[1]
    y2.lab <- pretty(y2lim,n=ny.ann)
    aty2 <- (y2.lab-y2lim[1])/diff(range(y2lim))*diff(range(y1lim))+
      y1lim[1]
    if (!is.null(h2)) h2 <- (h2-y2lim[1])/diff(range(y2lim))*
      diff(range(y1lim))+y1lim[1]
  }

  par(mfrow = c(ifelse(leg,fpp+1,fpp),1),
      mgp = mgp, mar = mar, oma=oma, cex=cex)

  if (Sys.info()["sysname"]!="Windows"){
    filetype <- postscript
    extension <- ".ps"
  }
  
  if (length(startP) != 1) stop("startP must be scalar!")
  upp <- upf*fpp
  
  if (slide) {
  ee <- end(y1)[1]
  nr <- 0
    
  for (ii in 0:(ceiling((end(y1)[1]+1-startP)/upp)-1)){
    if (ii%%each.fig!=0) next
    st <- startP+ii*upp
    nr <- nr+1
    plot.onepage(
       y1=y1, y2=y2, names1=names1, names2=names2,
       startP=st, upf=upf, fpp=fpp, overlap=overlap,
       x.ann=x.ann, dx.ann=dx.ann, dx.tick=dx.tick, 
       ny.ann=ny.ann, cex.ann=cex.ann,
       xlab=xlab, y1lab=y1lab, y2lab=y2lab, col.y1=col.y1, col.y2=col.y2,
       y2.lab = y2.lab, cex.lab=cex.lab,
       y1lim=y1lim, y2lim=y2lim, aty1=aty1, aty2=aty2,
       lty1=lty1, lty2=lty2, lwd1=lwd1, lwd2=lwd2, col1=col1, col2=col2,
       leg=leg, y1nam.leg=y1nam.leg, y2nam.leg=y2nam.leg,
       ncol.leg=ncol.leg, cex.leg=cex.leg,
       h1=h1, h2=h2, col.h1=col.h1, col.h2=col.h2,
       main=if (automain)
          paste("From",max(st,startP),"to",
                min(st+upp-1+round(overlap),ee+1)) else main,
       cex.main=cex.main,
       mgp=mgp, mar=mar, oma=oma, cex=cex, type=type)
      if (!is.null(filename)){
        fn <- paste(filename, formatC(nr,format = "d", width=3, flag=0),
                    extension, sep="")
        if (Sys.info()["sysname"]=="Windows")
          savePlot(filename=fn, type=filetype, ...) else
        dev.print(file=fn,device=filetype, ...)
      }
     print(answ <- readline(prompt="\nfor next plot: press <return>\nfor stopping plot: press <s><return>"))
    if (is.element(answ,c("s","S"))) return()
  }
} else {
  if (end(y1)[1]>start(y1)[1])
  plot.onepage(y1=y1, y2=y2, names1=names1, names2=names2,
               startP=startP, upf=upf, fpp=fpp, overlap=overlap,
               x.ann=x.ann, dx.ann=dx.ann, dx.tick=dx.tick, 
               ny.ann=ny.ann, cex.ann=cex.ann,
               xlab=xlab, y1lab=y1lab, y2lab=y2lab, y2.lab = y2.lab,
               col.y1=col.y1, col.y2=col.y2, cex.lab=cex.lab,
               y1lim=y1lim, y2lim=y2lim, aty1=aty1, aty2=aty2,
               lty1=lty1, lty2=lty2, lwd1=lwd1, lwd2=lwd2,
               col1=col1, col2=col2,
               leg=leg, y1nam.leg=y1nam.leg, y2nam.leg=y2nam.leg,
               ncol.leg=ncol.leg, cex.leg=cex.leg,
               h1=h1, h2=h2, col.h1=col.h1, col.h2=col.h2,
               main = if (automain)
                 paste("From",max(startP),"to", startP+upp-1) else main,
               cex.main=cex.main,
               mgp=mgp, mar=mar, oma=oma, cex=cex, type=type)
      if (!is.null(filename)){
        if (Sys.info()["sysname"]=="Windows")
          savePlot(filename=paste(filename,extension,sep=""),
                   type=filetype, ...) else
        dev.print(file=paste(filename,extension,sep=""),
                  device=filetype, ...)
      }
}
} #longts.plot

