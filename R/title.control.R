`title.control` <-
function(text = NULL,
                          cex = 1.5,
                          between = if(is.null(text)) 0 else 1)
  ## Author: Rene Locher
  ## Version: 2007-10-02
  
  {
    return(list(text = text,
                cex = cex,
                between = between))
  } ## title.control

