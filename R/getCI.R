#' Calculate 90% or 95% confidence intervals
#'
#' @description
#'
#' Calculates the upper and lower bound of a 90% confidence interval, based on the
#' value and standard error.
#'
#' @param .df data.frame with parameter estimates
#' @param .value column name to be used to calculate confidence interval
#' @param .se column name with standard errors
#' @param .ci specify 90 or 95 percent confidence interval (default 95%)
#'
#' @examples
#'
#' Theoph_ex <- Theoph %>% dplyr::mutate(stderr = conc/10)
#' getCI(.df = Theoph_ex, .value = "conc", .se = "stderr", .ci = 95)
#'
#' @export
getCI <- function(.df, .value = "value", .se = "se", .ci = 95){

  .df <- grouped_warn_ungroup(.df, as.character(match.call()[[1]]))

  .df %>%
    dplyr::mutate(lower = lowerCI(.est = .df[[.value]], .se = .df[[.se]], .ci = .ci),
                  upper = upperCI(.est = .df[[.value]], .se = .df[[.se]], .ci = .ci))
}
