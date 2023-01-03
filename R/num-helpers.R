#' Convert to numeric
#'
#' @param f character variable to be converted to numeric
#'
#' @keywords internal
asNum  <- function(f){
  return(as.numeric(as.character(f)))
}

#' Get even number
#'
#' @param x numeric value
#'
#' @keywords internal
getEvenNo = function(x){
 # if (x %% 2 == 0){
    x[which(x %% 2 == 0)]
  # } else {
  #   x[which(x %% 2 == 0)]
  #   stop("Not an even number")
  # }

}
