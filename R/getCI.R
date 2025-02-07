#' Calculate 90% or 95% confidence intervals
#'
#' @description
#'
#' Calculates the upper and lower bound of a 90% or 95% confidence interval, based on the
#' value and standard error.
#'
#' @param .df data.frame with parameter estimates
#' @param .value column name to be used to calculate confidence interval
#' @param .se column name with standard errors
#' @param .ci specify 90 or 95 percent confidence interval (default 95%)
#'
#' @keywords internal
getCI <- function(.df, .value = "value", .se = "se", .ci = 95){

  .df %>%
    dplyr::mutate(
      lower = calculate_ci(.df[[.value]], .df[[.se]], .ci = .ci)$lower,
      upper = calculate_ci(.df[[.value]], .df[[.se]], .ci = .ci)$upper,
      ci_level  = .ci
    )

}


#' Get the lower and upper intervals for a confidence interval
#' @param est parameter estimates
#' @param se standard error
#' @inheritParams getCI
#' @noRd
calculate_ci <- function(est, se, .ci = 95) {
  # Get the Z statistic for a two-tailed z-test
  alpha <- 1 - (.ci / 100)
  z_stat <- qnorm(1 - (alpha / 2))

  # Intervals
  lower <- est - z_stat * se
  upper <- est + z_stat * se

  list(
    lower = lower,
    upper = upper
  )
}

