"ipairs" <-
function(x,                 
                   pixs=1,            
                   zmax=NULL,         
                   colramp=IDPcolorRamp,            
                   border=FALSE,     
                   labels,            
                   cex.lab = NULL,
                   nlab=5,   
                   pty="s",
                   main=NULL,         
                   cex.main=par("cex.main"),
                   ...)
  ## Produces an image scatter plot matrix of largee datasets
  
  ## based on R function pairs V1.7
  ## Authors: Andreas Ruckstuhl and René Locher
  ## Version: 2005-02-01

{
  if(is.null(dim(x))) stop("x must be a data.frame or a matrix")
     
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  nc <- ncol(x)
  if (missing(labels)) {
    labels <- colnames(x)
    if (is.null(labels)) 
      labels <- paste("var", 1:nc)
  }

  ## factors are sorted and coerced into integers
  is.f <- sapply(x,is.factor)
  nf <- sum(is.f)
  if(nf>0) {
    ##integer coercing of data.frames does not work!
    lev.list <- sapply(x[is.f],levels,simplify=FALSE)
    for (i in which(is.f)) x[,i] <- as.integer(x[,i])
    x <- cbind(x[,!is.f],x[,is.f])
    labels <- c(labels[!is.f],labels[is.f])
    ## warning("factors have been put to the end and coerced into integers!")
  }

  is.f <- c(rep(FALSE,nc-nf),rep(TRUE,nf))
  lom <- matrix(1:nc^2, ncol=nc) + 4
  lom <- rbind(c(0,rep(3,nc),0), cbind(rep(2,nc),lom,rep(4,nc)),
               c(0,rep(1,nc),0))
  par(mar=rep(1,4), las=1, pty="m")

  w <- 3 * (par("cin")[2]*0.66) * 2.54
  lo <- layout(lom, width=c(lcm(w),rep(1,nc), lcm(2.5*w)),
               height=c(lcm(2*w),rep(1,nc), lcm(w)), respect=TRUE)
  ## layout.show(lo)
  ## return()

  h.LegendAndTitle(main,max(cex.lab,cex.main),border,colramp,zmax)
  
  cntsmax <- 0
  if (is.null(cex.lab)) {
    l.wid <- strwidth(labels, "user")
    cex.lab <- max(0.8, min(2, 0.9/max(l.wid)))
  }

  ## drawing scatterplots and axes  
  par(mar=rep(1,4), las=1)
  for (i in 1:nc){
    for (j in 1:nc) {
      plot(if(is.f[i]) range(x[, i],na.rm=TRUE)+0.5*c(-1,1) else
           range(x[, i],na.rm=TRUE),
           if(is.f[j]) range(x[, j],na.rm=TRUE)+0.5*c(-1,1) else
           range(x[, j],na.rm=TRUE),
           xlab="", ylab="", axes=FALSE, type="n", ...)
      box()

      if (i == 1 && (!(j%%2))) {## draw axes
        at <- pretty(x[, j],n=nlab)
        if(is.f[j]) {
          at <- at[(signif(at,dig=1)-at)<1e-3]
          axis(2, at=at,
               labels=abbreviate(lev.list[[j-nc+nf]][at],minl=2),
               xpd = NA)
        } else
        axis(2, at=at, xpd = NA)
      }

      if (i == nc && (j%%2 )) {## draw axes
        at <- pretty(x[, j],n=nlab)
        if(is.f[j]) {
          at <- at[(signif(at,dig=1)-at)<1e-3]
          axis(4, at=at,
               labels=abbreviate(lev.list[[j-nc+nf]][at],minl=2),
               xpd = NA)
        } else
        axis(4, at=at, xpd = NA, adj=1)
      }

      if (j == 1 && (!(i%%2))) {## draw axes
        at <- pretty(x[, i],n=nlab)
        if(is.f[i]) {
          at <- at[(signif(at,dig=1)-at)<1e-3]
          axis(3, at=at,
               labels=abbreviate(lev.list[[i-nc+nf]][at],minl=2),
               xpd = NA)
        } else
        axis(3, at=at, xpd = NA)
      }

      if (j == nc && (i%%2 ))  {## draw axes
        at <- pretty(x[, i],n=nlab)
        if(is.f[i]) {
          at <- at[(signif(at,dig=1)-at)<1e-3]
          axis(1, at=at,
               labels=abbreviate(lev.list[[i-nc+nf]][at],minl=2),
               xpd = NA)
        } else
        axis(1, at=at, xpd = NA)
      }
      
      if(i!=j){## do scatter plot
        cntsmax <- max(cntsmax,
                       h.image(x=x[, i], y=x[, j],
                               pixs=pixs, zmax=zmax,
                               colramp=colramp))  
      }
      
      else{## fill text into diagonal
        par(usr = c(0, 1, 0, 1))
        text(x=0.5, y=0.5, labels=labels[i], cex = cex.lab, font = 1)
      }
    }
  }

  return(cntsmax)
} # ipairs

