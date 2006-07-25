"plot.onepage" <-
function(y1, y2, names1, names2,
                         startP, upf, fpp, overlap,
                         ann.x, ann.dx, tick.dx, ann.ny,
                         xlab, y1lab, y2lab, y1.col, y2.col,
                         y2.lab, cex.lab,
                         y1lim, y2lim, aty1, aty2,
                         lty1, lty2, lwd1, lwd2, col1, col2,
                         leg, leg.y1nam, leg.y2nam,
                         leg.ncol, leg.cex=1.5,
                         h1, h2, h1.col, h2.col,
                         main, main.cex, mgp, oma, cex,type){
  ## internal function
  ## Author:  Rene Locher
  ## Version: 2006-07-24
  
  par.old <- par(no.readonly = TRUE)
  on.exit(par(par.old))
  options(warn=-1)
  on.exit(options(warn=0), add=TRUE)

  par(mfrow=c(ifelse(leg,fpp+1,fpp),1),
      mgp=mgp, mar=c(2,4,2,4)+.1, oma=oma, cex=cex)
   
  for (ff in 0:(fpp-1)) {
    st <- startP+ff*upf
    ee <- startP+(ff+1)*upf+overlap
    atxLab <- ts(seq(st,ee,ann.dx),start=st,freq=1/ann.dx)
    atxTick <- ts(seq(st,ee,tick.dx),start=st,freq=1/tick.dx)    
    st1 <- start(y1)
    st1 <- max(st,st1[1]+(st1[2]-1)/frequency(y1))
    ee1 <- end(y1)
    ee1 <- min(ee,ee1[1]+(ee1[2]-1)/frequency(y1))

    if (st1 < ee1) {
      err <- try(plot(window(y1,start=st1,end=ee1),
                      col=col1,lty=lty1, lwd=lwd1,  ylim=y1lim,
                      xlim=c(st,ee), type=type, plot.type="single",
                      xlab=xlab, ylab="", las=1,axes=F),
                 silent=TRUE)
      if (!is.null(err)) {
        if (regexpr("margins too large",geterrmessage())>0) {
          stop("\nToo many figures per page defined.\nChoose smaller value for 'fpp' and use slide=TRUE\n", call. = FALSE)} else
        stop(paste("\nUndefined error:\n",
                   geterrmessage(),sep=""), call. = FALSE)
      }
      
      if (!is.null(h1)) abline(h=h1, col=h1.col)
      
      if (!is.null(y2)) {
        st2 <- start(y2)
        st2 <- max(st,st2[1]+(st2[2]-1)/frequency(y2))
        ee2 <- end(y2)
        ee2 <- min(ee,ee2[1]+(ee2[2]-1)/frequency(y2))

        if (st2<ee2)
        for (ii in 1:ncol(y2))
          lines(window(y2[,ii],start=st2,end=ee2),col=col2[ii],
                lty=lty2[ii], lwd=lwd2, type=type)
        if (!is.null(h2)) abline(h=h2, col=h2.col)
       }
      
      ## Make sure that units labeled correctly
      atxLab <- window(atxLab,start=st1,end=ee1,frequ=1/ann.dx)
      if (is.null(ann.x)) lab <- atxLab else 
      lab <- window(ann.x,start=st1,end=ee1,freq=1/ann.dx)

      axis(1,at=atxTick,labels=F,tcl=-0.3)

      axis(1,at=atxLab,label=lab,tcl=-0.5)
      
      axis(2,aty1, col.ann=y1.col, col.axis=y1.col)
      mtext(text=y1lab,side=2,line=mgp[1],cex=cex*cex.lab,col=y1.col)
      
      if (!is.null(y2)) {
        axis(4,aty2, y2.lab, col.ann=y2.col, col.axis=y2.col)
        mtext(text=y2lab,side=4,line=mgp[1],cex=cex*cex.lab,col=y2.col)
      }
      box()
    } else frame()
  }

  if (leg) {
    plot(0:1,0:1,type="n",an=FALSE,axes=FALSE)
    cH <- strheight("A")
    cW <- strwidth("A")

    if (is.null(y2)) {
      if (is.null(leg.ncol)) leg.ncol <- ncol(y1)
      le <- legend(0.5,0.5,legend=names1,col=col1,lwd=lwd1,lty=lty1,
                   ncol=leg.ncol, xjust=0.5, yjust=0.5, cex=leg.cex,
                   bty="n",plot=FALSE)
      if (is.null(leg.y1nam)) legend(0.5,le$rect$h,
                                     legend=names1,
                                     col=col1,lwd=lwd1,lty=lty1,
                                     ncol=leg.ncol,
                                     xjust=0.5, yjust=1, cex=leg.cex,
                                     bty="n") else {
      le <- legend(0.5,le$rect$h,
                   legend=names1,
                   col=col1,lwd=lwd1,lty=lty1,
                   ncol=leg.ncol,
                   xjust=0.5, yjust=1, cex=leg.cex,
                   bty="n")
      text(0.5,le$rect$h+cH*leg.cex,
           leg.y1nam, cex=leg.cex, adj=0.5)
    }
    } else {
      if (is.null(leg.ncol)) leg.ncol <- max(ncol(y1),ncol(y2))
      if (is.null(leg.y1nam)) leg.y1nam <- "left axis:"
      if (is.null(leg.y2nam)) leg.y2nam <- "right axis:"
      text.w <- max(sapply(c(names1,names2),strwidth, cex=cex*leg.cex))
      
      le1 <- legend(0.5,0.5,legend=names1,col=col1,lwd=lwd1,lty=lty1,
                    ncol=leg.ncol, xjust=0.5, yjust=1, cex=leg.cex,
                    bty="n", text.width=text.w, plot=FALSE)
      le2 <- legend(0.5,0.5, legend=names2, col=col2,lwd=lwd2,lty=lty2,
                    ncol=leg.ncol, xjust=0.5, yjust=1, cex=leg.cex,
                    bty="n", text.width=text.w, plot=FALSE)

      leg.w <- min(max(le1$rect$w,le2$rect$w),1)

      par(xpd=NA)
      le1 <- legend(0.5-leg.w/2, le1$rect$h+le2$rect$h-2*cH,
                    legend=names1,col=col1,lwd=lwd1,lty=lty1,
                    ncol=leg.ncol, xjust=0, yjust=1, cex=leg.cex,
                    bty="n", text.width=text.w)
      le2 <- legend(0.5-leg.w/2, le2$rect$h-cH,
                    legend=names2,col=col2,lwd=lwd2,lty=lty2,
                    ncol=leg.ncol, xjust=0, yjust=1, cex=leg.cex,
                    bty="n", text.width=text.w)
      
      text(0.5-(leg.w+cW*leg.cex)/2, le1$text$y[1], leg.y1nam,
           cex=leg.cex, adj=1, col=y1.col)
      text(0.5-(leg.w+cW*leg.cex)/2, le2$text$y[1], leg.y2nam,
           cex=leg.cex, adj=1, col=y2.col)
    }
  }
  if (!is.null(main)) mtext(text=main,side=3,line=0,outer=T,cex=main.cex)
} #plot.onepage

