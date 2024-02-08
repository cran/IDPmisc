## val.rose.R

val.rose <- function(object)
  ## Validation function for rose-class
  ## Author: Rene Locher
  ## Version: 2005-10-17
  {
    if (length(object@cyclVar)!=nrow(object@rho))
      return("nrow(object) must be equal to length(cyclVar)\n")
    if (length(object@circle)!=1) return(paste("length of 'circle' is ",
                length(object@circle),", but must be 1!\n",sep=""))
    if (sum(is.na(object@cyclVar))>0)
      return("slot 'cyclVar' must not contain NAs!\n")
    if (min(object@cyclVar,rm.na=TRUE)<0 || max(object@cyclVar,rm.na=T)>=object@circle)
      return("The following condition must be valid: 0 <= 'cyclVar' < 'circle'!\n")
    if (!any(sort(object@cyclVar)==object@cyclVar))
      return("'cyclVar' must be sorted by increasing values!\n")
    if (any(duplicated(object@cyclVar)))
      return("All values of 'cyclVar' must be unique!\n")
    if (is.null(colnames(object@rho)))
        return("'rho' must have column names!\n")
    if (is.null(rownames(object@rho)))
        return("'rho' must have row names!\n")
    return(TRUE)
    } # val.rose
