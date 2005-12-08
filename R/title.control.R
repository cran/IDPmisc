"title.control" <-
function(text = NULL,
                          cex = 1.7,
                          between = if(is.null(text)) 0 else 1)
  ## Author: Rene Locher
  ## Version: 2005-12-06
  
  {
    return(list(text = text,
                cex = cex,
                between = between))
  } ## title.control

