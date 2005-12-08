"cart2clock" <-
function(x, y, circle)
  ## Author: Rene Locher
  ## Version: 2005-10-05
  ##
  ## converts clock coordinates to cartesian coordinates
  ## Attention: phi corresponds to the angle between y and the direction wanted,
  ## measured clockwise.
  ## This corresponds to an angle pi/2-phi in ordinary polar coordinates
  {
  phi <-(atan(x/y)/2/pi*circle + ifelse(y>0,circle,1.5*circle))%%circle
  return(data.frame(rho=sqrt(x*x+y*y),phi=phi))
}

