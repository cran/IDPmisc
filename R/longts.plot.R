"longts.plot" <-
function(y1, y2=NULL,
                        names1=NULL, names2=NULL,
                        startP=start(y1)[1],
                        upf=14, fpp=5, overlap=0.5, 
                        ann.x=NULL, ann.dx=1, tick.dx=0.25*ann.dx,
                        ann.ny=3,
                        xlab="",
                        y1lab="", y2lab="",
                        y1.col="black", y2.col="black",
                        cex.lab=par("cex.lab"),
                        y1lim=range(y1,na.rm=T,finite=TRUE),
                        y2lim=range(y2,na.rm=T,finite=TRUE),
                        lty1=1, lty2=2, lwd1=1, lwd2=2,
                        col1=NULL, col2=NULL,
                        leg=TRUE, leg.y1nam=NULL, leg.y2nam=NULL,
                        leg.ncol=NULL, leg.cex=1.5,
                        h1=NULL, h2=NULL,
                        h1.col="gray70", h2.col="gray70",
                        main=NULL, main.cex=par("main.cex"),
                        automain=TRUE,
                        mgp=c(1.7,0.7,0),
                        oma = if (automain) c(0,0,1,0) else par("oma"),
                        cex=par("cex"),
                        type="s", slide=FALSE, each.fig=1) {
  ## Author:  Rene Locher
  ## Version: 2006-07-05

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

  if (!is.null(main)) oma[3] <- max(oma[3],5)

  if (is.null(col1)) col1 <-
    c("blue","green4" ,"red", "darkorchid4", "black", 
      "deepskyblue","green","orange", "violetred", "grey50",
      "saddlebrown")[1:min(nrow(y1),11)]

  len <- ncol(y1)
  lty1 <- rep(lty1,len)[1:len]
  lwd1 <- rep(lwd1,len)[1:len]
  aty1 <- pretty(y1lim,n=ann.ny)

  if (!is.null(ann.x)) {
    if (length(ann.x)!=nrow(y1))
      stop("'ann.x' must have the same length as timeseries 'y1' or must be NULL!", call. = FALSE)
    ann.x <- ts(ann.x,start=start(y1),freq=fq1)
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
    y2.lab <- pretty(y2lim,n=ann.ny)
    aty2 <- (y2.lab-y2lim[1])/diff(range(y2lim))*diff(range(y1lim))+
      y1lim[1]
    if (!is.null(h2)) h2 <- (h2-y2lim[1])/diff(range(y2lim))*
      diff(range(y1lim))+y1lim[1]
  }

  if (length(startP) != 1) stop("startP must be scalar!")
  upp <- upf*fpp
  
  if (slide) {
  ee <- end(y1)[1]
    
  for (ii in 0:(ceiling((end(y1)[1]+1-startP)/upp)-1)){
    if (ii%%each.fig!=0) next
    st <- startP+ii*upp
    plot.onepage(
       y1=y1, y2=y2, names1=names1, names2=names2,
       startP=st, upf=upf, fpp=fpp, overlap=overlap,
       ann.x=ann.x, ann.dx=ann.dx, tick.dx=tick.dx, 
       ann.ny=ann.ny,
       xlab=xlab, y1lab=y1lab, y2lab=y2lab, y1.col=y1.col, y2.col=y2.col,
       y2.lab = y2.lab, cex.lab=cex.lab,
       y1lim=y1lim, y2lim=y2lim, aty1=aty1, aty2=aty2,
       lty1=lty1, lty2=lty2, lwd1=lwd1, lwd2=lwd2, col1=col1, col2=col2,
       leg=leg, leg.y1nam=leg.y1nam, leg.y2nam=leg.y2nam,
       leg.ncol=leg.ncol, leg.cex=leg.cex,
       h1=h1, h2=h2, h1.col=h1.col, h2.col=h2.col,
       main=if (automain)
          paste("From",max(st,startP),"to",
                min(st+upp-1+round(overlap),ee+1)) else main,
       main.cex=main.cex,
       mgp=mgp, oma=oma, cex=cex, type=type)
    
    print(answ <- readline(prompt="\nfor next plot: press <return>\nfor stopping plot: press <s><return>"))
    if (is.element(answ,c("s","S"))) return()
  }
} else {
  if (end(y1)[1]>start(y1)[1])
  plot.onepage(y1=y1, y2=y2, names1=names1, names2=names2,
               startP=startP, upf=upf, fpp=fpp, overlap=overlap,
               ann.x=ann.x, ann.dx=ann.dx, tick.dx=tick.dx,
               ann.ny=ann.ny,
               xlab=xlab, y1lab=y1lab, y2lab=y2lab, y2.lab = y2.lab,
               y1.col=y1.col, y2.col=y2.col, cex.lab=cex.lab,
               y1lim=y1lim, y2lim=y2lim, aty1=aty1, aty2=aty2,
               lty1=lty1, lty2=lty2, lwd1=lwd1, lwd2=lwd2,
               col1=col1, col2=col2,
               leg=leg, leg.y1nam=leg.y1nam, leg.y2nam=leg.y2nam,
               leg.ncol=leg.ncol, leg.cex=leg.cex,
               h1=h1, h2=h2, h1.col=h1.col, h2.col=h2.col,
               main = if (automain)
                 paste("From",max(startP),"to", startP+upp-1) else main,
               main.cex=main.cex,
               mgp=mgp, oma=oma, cex=cex, type=type)
}
} #longts.plot

