#' Confidence interval bounds
#'
#' @param est parameter estimates
#' @param se standard error
#'
#' @keywords internal
lower95CI <- function(est, se){
  est - 1.96*se
}

#' @rdname lower95CI
#' @keywords internal
upper95CI <- function(est, se){
  est + 1.96*se
}

#' @rdname lower95CI
#' @keywords internal
lower90CI <- function(est, se){
  est - 1.64*se
}

#' @rdname lower95CI
#' @keywords internal
upper90CI <- function(est, se){
  est + 1.64*se
}
