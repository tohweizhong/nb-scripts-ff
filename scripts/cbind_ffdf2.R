cbind_ffdf2 <- function(d1, d2){
  D1names <- colnames(d1)
  D2names <- colnames(d2)
  mergeCall <- do.call("ffdf", c(physical(d1), physical(d2)))
  colnames(mergeCall) <- c(D1names, D2names)
  mergeCall
}