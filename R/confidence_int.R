#' Confidence interval bounds
#'
#' @param .est parameter estimates
#' @param .se standard error
#' @param .ci confidence interval type
#'
#' @keywords internal
lowerCI <- function(.est, .se, .ci){
  if (.ci == 90) {
    .est - 1.64*.se
  }

  if (.ci == 95) {
    .est - 1.96*.se
  }

  return(.est)
}

#' @rdname lowerCI
#' @keywords internal
upperCI <- function(.est, .se, .ci){
  if (.ci == 90) {
    .est + 1.64*.se
  }

  if (.ci == 95) {
    .est + 1.96*.se
  }

  return(.est)
}
