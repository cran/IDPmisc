"general.control" <-
function(mar = rep(0.3,4),
                            stacked = FALSE,
                            rev.col = FALSE,
                            cex = 1,
                            col = NULL,
                            lty = 1:3,
                            lwd = 1)
  ## Author: Rene Locher
  ## Version: 2005-12-06
  
  {
    return(list(rose =
                list(rad = NULL,
                     x = NULL,
                     y = NULL),
                mar = mar,
                stacked = stacked,
                rev.col = rev.col,
                cex = cex,
                col = col,
                lty = lty,
                lwd = lwd))
  } ## general.control

