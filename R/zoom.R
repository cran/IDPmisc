"zoom" <-
function(fun = usa, zoom.col="red", delay=3, ...){
  ## 
  ## fun       plotting function
  ## zoom.col  color of clicked points
  ## delay     number of sec during which the 2nd zooming point is shown
  ##           before zoomed figure ist drawn
  ## ...       Arguments for plotting function
  ##
  ##
  ## Author: Rene Locher
  ## Version 25.01.05
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  options(locatorBell=FALSE)
  cat("Click mouse at corners of zoom area.\nRight click to stop zooming")

  fun(...)
  while(TRUE) {
    par(xpd=NA)

    p1 <- locator(n = 1, ty="p", col=zoom.col)
    if(is.null(p1$x)){
      cat("\n")
      break
    }
    print(abline(v=p1$x,col=zoom.col))
    print(abline(h=p1$y,col=zoom.col))
    
    p2 <- locator(n = 1, ty="p", col=zoom.col)
    if(is.null(p2$x)){
      cat("\n")
      break
    }
    print(abline(v=p2$x,col=zoom.col))
    print(abline(h=p2$y,col=zoom.col))
    Sys.sleep(delay)

    xx <- sort(c(p1$x,p2$x))
    yy <-  sort(c(p1$y,p2$y))
    
    ## check if zooming out 
    usr <- par()$usr

    ## zooming out in x direction
    if (xx[1]<usr[1]) xx[1] <- usr[1]-diff(range(usr[1:2]/3))
    if (xx[2]<usr[1]) xx[2] <- usr[1]-diff(range(usr[1:2]/3))
    if (xx[1]>usr[2]) xx[1] <- usr[2]+diff(range(usr[1:2]/3))
    if (xx[2]>usr[2]) xx[2] <- usr[2]+diff(range(usr[1:2]/3))

    ## zooming out in y direction
    if (yy[1]<usr[3]) yy[1] <- usr[3]-diff(range(usr[3:4]/3))
    if (yy[2]<usr[3]) yy[2] <- usr[3]-diff(range(usr[3:4]/3))
    if (yy[1]>usr[4]) yy[1] <- usr[4]+diff(range(usr[3:4]/3))
    if (yy[2]>usr[4]) yy[2] <- usr[4]+diff(range(usr[3:4]/3))
    
    xlim <- range(xx)
    ylim <- range(yy)
    cat("xlim=", xlim, "ylim=", ylim, "\n")
    par(xpd=FALSE)
    fun(..., xlim = xlim, ylim = ylim)
  }

}  ## zoom

