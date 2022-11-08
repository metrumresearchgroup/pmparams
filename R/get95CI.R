#' Calculate 95% confidence intervals
#'
#' @description
#'
#' Calculates the upper and lower bound of a 95% confidence interval, based on the
#' value and standard error.
#'
#' @param df data.frame with parameter estimates
#' @param .value column name to be used to calculate confidence interval
#' @param .se column name with standard errors
#'
#' @examples
#'
#' Theoph_ex <- Theoph %>% dplyr::mutate(stderr = conc/10)
#' get95CI(.df = Theoph_ex, .value = conc, .se = stderr)
#'
#' @export
get95CI <- function(.df, .value = value, .se = se){
  .df %>%
    dplyr::mutate(lower = lower95CI({{.value}}, {{.se}}),
                  upper = upper95CI({{.value}}, {{.se}}))
}
