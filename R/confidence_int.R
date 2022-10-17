#' Confidence interval bounds
#'
#' @param est parameter estimates
#' @param se standard error
#'
#' @keywords internal
lowerCI <- function(est, se){
  est - 1.96*se
}

#' @rdname lowerCI
#' @keywords internal
upperCI <- function(est, se){
  est + 1.96*se
}
