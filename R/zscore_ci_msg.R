#' Generate message that confidence interval and z-score do not match
#'
#' @description
#'
#' Output message if confidence interval and z-score do not match for `param_notes`
#'
#' @keywords internal
zscore_ci_msg <- function(.ci, .zscore){
  message(
    glue::glue("Confidence interval and z-score provided do not match. The z-score that corresponds to {.ci}% CI will be used (z-score = {.zscore})")
  )
}
