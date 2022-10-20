#' Calculate 90% confidence intervals
#'
#' @description
#'
#' Calculates the upper and lower bound of a 90% confidence interval, based on the
#' value and standard error.
#'
#' @param df data.frame with parameter estimates
#' @param .value column name to be used to calculate confidence interval
#' @param .se column name with standard errors
#'
#' @examples
#'
#' Theoph_ex <- Theoph %>% dplyr::mutate(stderr = conc/10)
#' get95CI(.df = Theoph_ex, .value = "conc", .se = "stderr")
#'
#' @export
get90CI <- function(.df, .value = "value", .se = "se"){
  .df %>%
    dplyr::mutate(lower = lower90CI(.df[[.value]], .df[[.se]]),
                  upper = upper90CI(.df[[.value]], .df[[.se]]))
}
