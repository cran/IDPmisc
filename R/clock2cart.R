"clock2cart" <-
function(rho, phi, circle)
  ## Author: Rene Locher
  ## Version: 2005-10-05
  ##
  ## converts clock coordinates to cartesian coordinates
  ## Attention: phi corresponds to the angle between y and the direction wanted,
  ## measured clockwise.
  ## This corresponds to an angle of pi/2-phi in Ordinary polar coordinates  
{
  return(data.frame(x=rho*sin(phi/circle*2*pi),
                    y=rho*cos(phi/circle*2*pi)))
}

