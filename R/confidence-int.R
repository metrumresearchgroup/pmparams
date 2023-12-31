#' Confidence interval bounds
#'
#' @param .est parameter estimates
#' @param .se standard error
#' @param .ci confidence interval type
#'
#' @keywords internal
lowerCI <- function(.est, .se, .ci, .zscore = NULL){
  if (.ci == 90) {
   .est =  .est - 1.64*.se
  }

  if (.ci == 95) {
    .est = .est - 1.96*.se
  }

  if (!(.ci %in% c(90,95)) & !is.null(.zscore)) {
    .est = .est -.zscore*.se
  }

  return(.est)
}

#' @rdname lowerCI
#' @keywords internal
upperCI <- function(.est, .se, .ci, .zscore = NULL){
  if (.ci == 90) {
    .est = .est + 1.64*.se
  }

  if (.ci == 95) {
    .est = .est + 1.96*.se
  }

  if (!(.ci %in% c(90,95)) & !is.null(.zscore)) {
    .est = .est + .zscore*.se
  }

  return(.est)
}
