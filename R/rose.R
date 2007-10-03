"val.rose" <-
function(x)
  ## Validation function for rose-class
  ## Author: Rene Locher
  ## Version: 2005-10-17
  {
    if (length(x@cyclVar)!=nrow(x@rho))
      return("nrow(x) must be equal to length(cyclVar)\n")
    if (length(x@circle)!=1) return(paste("length of 'circle' is ",
                length(x@circle),", but must be 1!\n",sep=""))
    if (sum(is.na(x@cyclVar))>0)
      return("slot 'cyclVar' must not contain NAs!\n") 
    if (min(x@cyclVar,rm.na=T)<0 || max(x@cyclVar,rm.na=T)>=x@circle)
      return("The following condition must be valid: 0 <= 'cyclVar' < 'circle'!\n")
    if (!any(sort(x@cyclVar)==x@cyclVar))
      return("'cyclVar' must be sorted by increasing values!\n")
    if (any(duplicated(x@cyclVar)))
      return("All values of 'cyclVar' must be unique!\n")
    if (is.null(colnames(x@rho)))
        return("'rho' must have column names!\n")
    if (is.null(rownames(x@rho)))
        return("'rho' must have row names!\n")
    return(TRUE)
    } # val.rose

"rose" <-
function(x, subset=NULL, 
                 cyclVar=NULL, circle=NULL, n.cyclVar=8, 
                 cut=NULL, labels=NULL, breaks=NULL, 
                 include.lowest = FALSE, right = TRUE, dig.lab=2, 
                 warn=TRUE, FUN=mean, ...)
  ## Author: Rene Locher
  ## Version: 2005-10-18
  ##
  ## Calculates aggregates as function of cyclic variable like direction or hour
  ##  of day and, possibly, a second variable for splitting the data.
  ## Examples: The mean value of ozone concentration as a function of wind 
  ##           direction and wind speed.
  ##           The mean value of ozone, nitrogen dioxide, nitrogen oxid, etc. as
  ##           a function of hour of day.
  ##           The quantiles of ozone concentration as a function of wind
  ##           direction
  ##
  ## Details:
  ## Only selected combinations of the variables x, cyclVar, cut and
  ## FUN output are allowed.
  ## allowed combinations:
  ## x with ncol(x) >= 1: cyclVar, FUN output of length 1
  ## x with ncol(x) =  1: cyclVar, FUN output of length 1 and more
  ## x with ncol(x) =  1: cyclVar, FUN output of length 1, cut
  ##
  ##
  ## Output:
  ## The result is stored in a 2-dimensional matrix 'rho' with the rows
  ## corresponding to the values of the bin centers of the cyclic variable.
  ##
  ## The different columns correspond to
  ## the splitting levels of the second variable or to
  ## the different columns of x, when x has more than one column, or to
  ## the different elements of the output vector of FUN.
  ##
  ## The centers of the cyclic variable are stored in the vector 'cyclVar'
  ##
  
{
  lab <- deparse(substitute(x))

  if (is.vector(x)) {
    x <- as.matrix(x)
    colnames(x) <- lab 
  } else  if (!(is.matrix(x))) x <- as.matrix(x) else
  stop("'x' must be a matrix, a data.frame or a vector!\n")

  if(is.null(circle)) stop("'circle' is not defined!\n")

  if (is.null(cyclVar)) stop("'cyclVar' is not defined!\n") else
  if (!is.numeric(cyclVar)) stop("'cyclVar' must be numeric!\n") else
  if (is.factor(cyclVar)) stop("'cyclVar' must not be a factor!\n") else
  if (sum(cyclVar<0,na.rm=TRUE)+sum(cyclVar>=circle,na.rm=TRUE)>0)
    stop("The following condition must be full filled: \n0 <= 'cyclVar' < 'circle'\n")
  
  if (nrow(x) != length(cyclVar))
    stop("x and cyclVar have incompatible dimensions!\n")      
  if (is.null(colnames(x)))
    colnames(x) <- paste("x",1:ncol(x),sep="")

  if (!is.null(subset)) {
    subset[is.na(subset)] <- F
    cyclVar <- cyclVar[subset]
    if (!is.null(cut)) cut <- cut[subset]
    x <- x[subset,,drop=FALSE]
  }
  
  ii <- is.na(cyclVar)
  nna <- sum(ii)
  if (sum(nna)>0&warn){
    warning("cyclVar' must not contain NAs! ",nna," observations are omitted!!\n")
    x <- x[!ii,,drop=FALSE]
    cyclVar <- cyclVar[!ii]
    if (!is.null(cut)) cut <- cut[!ii]
  }

  ## make sure that label are numerics
  cyclVar.lab <- circle/n.cyclVar*(0:(n.cyclVar-1))

  ## labels are the center of the category
  cyclVar.f <- cut((cyclVar+circle/n.cyclVar/2)%%circle,
               breaks=c(cyclVar.lab,circle),
               labels=cyclVar.lab,include.lowest=T)

  if (ncol(x)>1) {
    if ((!is.null(cut)|!is.null(breaks))&warn)
      warning("When x has more than 1 column, 'cut' and 'breaks' are not used!\n")
    res <- aggregate(x, by=list(cyclVar=cyclVar.f), FUN=FUN, ...)
    res$cyclVar <- as.numeric(as.character(res$cyclVar))

    ## make sure that all possible values are included!
    res <- merge(cyclVar.lab, res, by.x=1, by.y="cyclVar", all=TRUE)
    res <- as.matrix(res[,-1])
  } else if (is.null(cut)) {
    ## statistics with possibly more than a single value per level of cyclVar.f
    res <- tapply(unlist(x), INDEX=list(cyclVar=cyclVar.f), FUN=FUN, ...)
    len <- sapply(res,length)
    if (length(len)>1) {
      if (diff(range(len))!=0) {
        ii <- len == 0
        if (sum(ii)>0) res <- res[!ii]
        if (diff(range(sapply(res,length)))!=0) {
          print(res)
          stop("'FUN' must return a vector of fixed length!\n")
        } 
      }
    }
    
    cnam <- names(res[[1]])
    cyclVar <- as.numeric(names(res))
    res <- cbind(cyclVar,t(matrix(unlist(res),ncol=length(res))))

    ## make sure that all possible values are included!
    res <- merge(cyclVar.lab, res, by.x=1, by.y=1, all=TRUE)
    res <- as.matrix(res[,-1])

    if (!is.null(cnam)) colnames(res) <- cnam
    else colnames(res) <- paste("V",1:ncol(res),sep="")
  } else { ## 2 D statistics with one column
    if (nrow(x)!=length(cut)) stop("The vector 'cut' must have the same length as vector'x'\n")
    if (is.factor(cut)) {
      cut.f <- cut} else
    if (is.logical(cut)) {
      cut.f <- factor(cut)} else {
      if (is.null(breaks)) {
        breaks <- pretty(c(cut,max(cut)+diff(range(cut)*0.01)),n=3)
      }
      cut.f <- cut(cut, breaks=breaks, labels=labels, dig.lab=dig.lab,
                   include.lowest=include.lowest, right = right)
    }

    if (length(eval(FUN(x[,1])))>1)
      stop("When 'cut' is not NULL, 'FUN' must return a scalar!\n")

    nout <- sum(is.na(cut.f)&!is.na(x)&!is.na(cut))
    if (warn & nout>0)
      warning(nout," values are out of range of breaks!\n")
    
    agg <- aggregate(x, by=list(cyclVar=cyclVar.f,cut=cut.f), FUN=FUN, ...)

    ## Make sure that all combination are in the matrix
    res <- expand.grid(cyclVar=cyclVar.lab,cut=levels(cut.f))
    res <- merge(res,agg,all.x=TRUE)

    res <- res[order(as.numeric(res$cut),as.numeric(res$cyclVar)),]

    res <- matrix(res[,3],nrow=n.cyclVar, byrow=FALSE)

    colnames(res) <- if (is.null(labels)) levels(cut.f) else labels
  }
  
  rownames(res) <- cyclVar.lab
  return(new("rose",rho=res,cyclVar=cyclVar.lab,circle=circle))
} ## rose


`plot.rose` <- function (x,
                         transf = function(x) sqrt(x),
                         subset.col = NULL,
                         warn = TRUE, 
                         general = general.control(),
                         grid = grid.control(),
                         title = title.control(),
                         key = key.control())
  ## Author: Rene Locher
  ## Version: 2007-10-02
  ##
  ## NA are plotted as 0

{
  if (!is.unit(general$mar))
    general$mar <- unit.c(unit(general$mar*grid$cyclVar$cex,
                               units="char"))
  if (!is.unit(key$between)) key$between <-
    unit(key$between*grid$cyclVar$cex, units="char")

  ## convert distances in viewport independent units
  pushViewport(viewport(gp=gpar(cex=general$cex)),recording=FALSE)
  general$mar <- convertWidth(general$mar,"mm")
  key$between <- convertWidth(key$between,"mm")
  popViewport()
  
  if (!is.null(key$x)&&!is.unit(key$x)) key$x <- unit(key$x,units="snpc")

  ## checking if length of cyclVar$lab is compatible with ray$n
  if ((grid$ray$n %% length(grid$cyclVar$lab))) stop("'ray.n' is not a muliple of length of 'cyclVar.lab'!\n")

  rho <- x@rho
  if (!is.null(subset.col)) { # chose columns to be plotted
    if (is.numeric(subset.col)) {
      if (any(cc <- !is.element(subset.col,1:ncol(rho)))) 
        stop(paste("", paste(subset.col[cc],collapse=", "),
                   "in 'subset.col' define(s) no valid column of 'x'!"))
    } else
    if (any(cc <- !is.element(subset.col,colnames(rho))))
      stop(paste("", paste(subset.col[cc],collapse=", "),
                 "in 'subset.col' define(s) no valid column of 'x'!"))
    rho <- rho[,subset.col,drop=FALSE]
  }

  ## check if transformation function is compatible with data
  if (any(!is.finite(transf(as.vector(rho))) &
          !is.na(as.vector(rho))))
    stop("Transformation function 'transf' is incompatible with data. You possibly have tried to apply the square root transformation to negativ data.\n")

  ## make sure that stacked and unstacked case have
  ## the same order in the legend
  if (!general$stacked) general$rev.col <- !general$rev.col 
  if (general$rev.col) rho <- rho[,ncol(rho):1,drop=FALSE]

  ## NAs are interpreted as 0 (when counts or percentages)
  ## in other cases this is arbitrary
  i.na <- is.na(rho)
  if (sum(i.na)) {
    rho[i.na] <- 0 
    if (warn) warning("NAs encountered in rho! NAs are set to 0. Be careful with interpretation!\n",
            call. = FALSE)
  }
  
  if (general$stacked) {
    if (min(rho,na.rm=TRUE)<0)
      stop("Stacked roses make sense only for positive variables like counts,  proportions and concentrations!")
    rho <- t(apply(rho, MAR=1, cumsum))[,ncol(rho):1]
  } ## stacked

  ## calculating the labels for the (main) circles
    if (is.null(grid$circ$r)) {
      if (is.null(grid$ray$lim)) {
        grid$circ$value <- pretty(c(0,rho), n = grid$circ$n)
      } else grid$circ$value <- pretty(c(grid$ray$lim, n = grid$circ$n))
    } else grid$circ$value <- grid$circ$r
  
  nc <- ncol(rho)
  
  ## choosing adequate colors for displaying the data
  if(general$stacked){ ## well blended colors
    if (is.null(general$col)) general$col <-
      IDPcolorRamp(nc,
                   colInt = data.frame(
                     h = c(0.6, 0.55, 0.45, 0.25), 
                     s = c(0.5, 0.55, 0.55, 0.55),
                     v = c(0.92, 0.92, 0.92,0.92)),
                   fr     = c(0.4,0.3))
  } else { ## distinct colors
    if (is.null(general$col))
      general$col <- c("blue","green4" ,"red", "darkorchid4", "black", 
         "deepskyblue","green","orange", "violetred",
         "grey50", "saddlebrown")
  }

  ## constructing a col vector of correct length
  ll <- length(general$col)%/%nc + 1
  if (length(general$col)>nc) general$col <- rep(general$col,ll)[1:nc]

  ## constructing a lty vector of correct length  
  ll <- length(general$lty)%/%nc + 1
  if (length(general$lty)>nc) general$lty <- rep(general$lty,ll)[1:nc]

  ## define ray.lim if NULL or
  ## trim circ$value if ray.lim defines smaller range
  if (is.null(grid$ray$lim))
    grid$ray$lim <- range(grid$circ$value) else 
  grid$circ$value <- grid$circ$value[grid$circ$value >= grid$ray$lim[1] &
                                grid$circ$value<=grid$ray$lim[2]]
  if (grid$ray$lim[1]!=0)
    warning("Be careful! The center of the rose is not 0, which might be misleading to the reader.\n")

  ## calculating circle radius in transformated native coordinates:
  ## center of circles is trans(ray.lim[1])
  ## untransformed radii are retained in circ$value
  grid$circ$r <- transf(grid$circ$value) - transf(grid$ray$lim[1])

  ## adjust circ.n to the actual number of circles
  grid$circ$n <- length(grid$circ$r)
  
  ## subcircles are plotted when either circ.sub.n or circ.sub.rle
  ## are defined
  grid$circ$sub$plot <- (!is.null(grid$circ$sub$n) |
                         !is.null(grid$circ$sub$r))

  if (grid$circ$sub$plot){
    if (length(grid$circ$value)>1) {
      if (is.null(grid$circ$sub$r))
        grid$circ$sub$r <-
          transf(seq(grid$circ$value[1],
                     by = (diff(grid$circ$value[1:2]) / grid$circ$sub$n),
                     to = grid$circ$value[length(grid$circ$value)])) -
      transf(grid$ray$lim[1]) else
      grid$circ$sub$r <-
        transf(grid$circ$sub$r) - transf(grid$ray$lim[1])
    } else {
      grid$circ$sub$plot <- FALSE
      warning("Definitions of 'circ.n', 'circ.r' incompatible with definitions for subcircles. No subcircles are plotted.\n")
    }
  }
  
  if (key$plot&&nc>1) {## plot legend
    if (is.null(key$lab)) key$lab <- colnames(rho)
    
    if (general$stacked)
      key.grob <- draw.leg(key = list(rectangles = list(
                                      col=general$col,
                                      size = 2,
                                      lwd = 0.5),
                           text = list(key$lab),
                           cex = general$cex,
                           between = 1,
                           between.rows = 0.5,
                           between.title = 0.5*grid$cyclVar$cex,
                           title = key$title,
                           adj.title = 0,
                           cex.title = grid$cyclVar$cex,
                           transparent = TRUE))
    else
      key.grob <- draw.leg(key = list(lines = list(
                                        col = general$col,
                                        lwd = general$lwd,
                                        lty = general$lty,
                                        size = 3),
                             text = list(key$lab),
                             between = 1,
                             between.rows = 0.5,
                             between.title = 0.5*grid$cyclVar$cex,
                             cex = general$cex,
                             title = key$title,
                             adj.title = 0,
                             cex.title = grid$cyclVar$cex,
                             transparent = TRUE))

    ## convert distances in viewport independent units
    pushViewport(viewport(gp=gpar(cex=general$cex)),recording=FALSE)
    keyWidth <- grobWidth(key.grob)
    if (!is.null(key$title))
      keyWidth <- convertWidth(max(grobWidth(key.grob),
                    unit(grid$cyclVar$cex,
                         "strwidth",key$title)),"mm") else
    keyWidth <- convertWidth(grobWidth(key.grob),"mm")
    
    popViewport()
  } else
  keyWidth <- unit(0,"mm")

  ##------------ 

  if (general$rose$auto) {
  ## calculation the proper rose$rad when it is not defined explicitely  
     delta <- unit(max(sapply(grid$cyclVar$lab,nchar)),"lines")
     general$rose$rad <- unit(0.5,"snpc") - 0.5*keyWidth -
       0.5*key$between - delta
     rose.x <-  rose.y <- general$rose$rad+delta
   } else delta <- unit(max(sapply(grid$cyclVar$lab,nchar)),"lines")
      
  if (is.null(general$rose$x)) rose.x <- general$rose$rad+delta else
  rose.x <- general$rose$x

  if (is.null(general$rose$y)) rose.y <- general$rose$rad+delta else
  rose.y <- general$rose$y
      
  vp.rose <- viewport(name="vp.rose",
                      x = rose.x,
                      y = rose.y,
                      width = 2*general$rose$rad,
                      height = 2*general$rose$rad,
                      xscale = c(transf(grid$ray$lim)[1]-
                        transf(grid$ray$lim)[2],
                        transf(grid$ray$lim)[2]-
                        transf(grid$ray$lim)[1]),
                      yscale = c(transf(grid$ray$lim)[1]-
                        transf(grid$ray$lim)[2],
                        transf(grid$ray$lim)[2]-
                        transf(grid$ray$lim)[1]),
                      just = c("center","center"),
                      gp = gpar(cex=general$cex),
                      clip = "off")

  pd <- plotdat(rho = rho,
                cyclVar = x@cyclVar,
                circle = x@circle,
                transf = transf,
                vp = vp.rose,
                general = general,
                grid = grid,
                title = title)
  
  if (general$rose$auto) {
  ## readjustment of rose$rad    
    if (convertWidth(key$between,"mm",valueOnly=TRUE)>0)
      delta <- 0.5*key$between else
    delta <- 0*key$between
    
    rad1 <- convertWidth(0.5*unit(1,"npc") - 0.5*keyWidth -
                         0.5*sum(pd$labSpace[c(2,4)]) -
                         0.5*sum(general$mar[c(2,4)]) - delta, "mm")
    rad2 <- convertHeight(0.5*unit(1,"npc") -
                          0.5*sum(pd$labSpace[c(1,3)]) -
                          0.5*sum(general$mar[c(1,3)]) - delta, "mm")
    
    general$rose$rad <- min(unit.c(rad1,rad2))
  }      

  if (is.null(general$rose$x))
    rose.x <- general$rose$rad+pd$labSpace[2] + general$mar[2] else
  rose.x <- general$rose$x

  if (is.null(general$rose$y))
    rose.y <- general$rose$rad+pd$labSpace[1] + general$mar[1] else
  rose.y <- general$rose$y
 
  vp.rose <- viewport(name="vp.rose",
                      x = rose.x,
                      y = rose.y,
                      width = 2*general$rose$rad,
                      height = 2*general$rose$rad,
                      xscale = c(transf(grid$ray$lim)[1]-
                        transf(grid$ray$lim)[2],
                        transf(grid$ray$lim)[2]-
                        transf(grid$ray$lim)[1]),
                      yscale = c(transf(grid$ray$lim)[1]-
                        transf(grid$ray$lim)[2],
                        transf(grid$ray$lim)[2]-
                        transf(grid$ray$lim)[1]),
                      just = c("center","center"),
                      gp = gpar(cex=general$cex),
                      clip = "off")

  pushViewport(vp.rose)
  grid.draw(rose.grob(pdat = pd,
                      general = general,
                      grid = grid,
                      title = title))
  upViewport()
    
 if (key$plot&&nc>1) {## plot legend
   vp.key <-  viewport(x = if (is.null(key$x))
                             2*general$rose$rad + general$mar[2] +
                             sum(pd$labSpace[c(2,4)]) + 
                             key$between else key$x,
                       y = general$mar[1],
                       default.units = "npc",
                       width = grobWidth(key.grob),
                       height = grobHeight(key.grob),
                       just = c("left","bottom"),
                       name = "vp.key")
    pushViewport(vp.key)
    grid.draw(key.grob)
    upViewport()
  }
} ## plot.rose


setClass("rose",
         representation(rho="matrix",
                        cyclVar="numeric",
                        circle="numeric"),
         validity=val.rose)

setMethod("plot", signature(x = "rose",y = "missing"), plot.rose)
