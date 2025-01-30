#' Calculate percentiles
#'
#' @description
#'
#' Calculates the upper and lower bound of a 90% or 95% confidence interval,
#' interquartile range, or any number of percentiles based on the value and
#' standard error.
#'
#' @param .boot data.frame with parameter estimates
#' @param .ci the confidence interval. A value from 1 to 100 denoting the percent
#'  confidence interval, or `"iqr"` (interquartile range). Default is `95`.
#' @param .na.rm logical (T/F). If `TRUE`, any `NA` and `NaN`'s are removed
#'   before the quantiles are computed. Passed to [stats::quantile()].
#'
#' @keywords internal
getBootPercentiles <- function(
    .boot,
    .ci = 95,
    .na.rm = TRUE
){
  checkmate::assert_true(length(.ci) == 1)
  if (tolower(.ci) == "iqr") .ci <- 50
  .ci <- as.numeric(.ci)
  if(.ci < 1 || .ci > 99){
    rlang::abort("`.ci` must be between 1 and 99 if numeric ('iqr' also acceptable)")
  }

  checkmate::assert_logical(.na.rm)

  comp_df <- .boot %>%
    dplyr::select(dplyr::starts_with(c("THETA", "SIGMA", "OMEGA")))

  quantile_fn <- function(x)  {
    stats::quantile(x, probs = .percentiles, na.rm = .na.rm)
  }

  # Convert CI to percentiles
  .percentiles <- get_percentiles_from_ci(.ci)

  # Calculate quantiles for each column
  comp_df <- comp_df %>%
    dplyr::reframe(
      dplyr::across(.cols = dplyr::everything(), .fns = quantile_fn)
    ) %>% t() %>% as.data.frame() %>%
    tibble::rownames_to_column() %>%
    tibble::as_tibble()

  # Set names and tabulate CI
  comp_df <- comp_df %>% dplyr::transmute(
    parameter_names = .data$rowname,
    lower = .data$V1,
    value = .data$V2,
    upper = .data$V3,
    ci_level = .ci
  )

  return(comp_df)
}

#' Function to get percentiles from confidence interval
#' @param .ci numeric value from 1 to 100 (character representations accepted)
#' @noRd
get_percentiles_from_ci <- function(.ci) {
  .ci <- as.numeric(.ci) / 100
  lower <- (1 - .ci) / 2
  upper <- 1 - lower
  return(c(lower, 0.5, upper))
}
