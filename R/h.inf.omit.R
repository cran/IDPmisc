"h.inf.omit" <-
function(x){
  ## internal function for ipairs
  ## works up to now only for numeric vectors!
 x <- x[is.finite(x)]
}## h.inf.omit

