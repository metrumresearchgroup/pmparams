#' Convert to numeric
#'
#' @param f character to be converted to numeric ???
#'
#' @keywords internal
asNum  <- function(f){
  return(as.numeric(as.character(f)))
}

#' Get even number
#'
#' @param x numeric value ???
#'
#' @keywords internal
getEvenNo = function(x){
  x[which(x %% 2 == 0)]
}
