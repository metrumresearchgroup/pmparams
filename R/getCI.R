#' Calculate 90% or 95% confidence intervals
#'
#' @description
#'
#' Calculates the upper and lower bound of a confidence interval, based on the
#' value and standard error.
#'
#' @param .df A data.frame with parameter estimates
#' @param .value Column name to be used to calculate confidence interval
#' @param .se Column name with standard errors
#' @param .ci Confidence interval. A value from 1 to 99 denoting the percent
#'  confidence interval. Default is `95`.
#'
#' @keywords internal
getCI <- function(.df, .value = "value", .se = "se", .ci = 95){

  if (!checkmate::test_integerish(.ci, lower = 1, upper = 99, len = 1)){
    rlang::abort("`.ci` must be between 1 and 99")
  }

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

