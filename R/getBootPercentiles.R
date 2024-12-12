#' Calculate percentiles
#'
#' @description
#'
#' Calculates the upper and lower bound of a 90% or 95% confidence interval,
#' interquartile range, or any number of percentiles based on the value and
#' standard error.
#'
#' @param .df data.frame with parameter estimates
#' @param .ci the confidence interval. A value from 1 to 100 denoting the percent
#'  confidence interval, or `"iqr"` (interquartile range). Default is `95`.
#' @param .percentiles optional vector of all percentiles (defaults to `NULL`).
#'  If specified `.ci` will be ignored. E.g., `.percentiles = c(0.025, 0.5, 0.975)`
#'  is the equivalent of `.ci = 95`.
#' @param .na.rm logical (T/F). If `TRUE`, any `NA` and `NaN`'s are removed
#'   before the quantiles are computed. Passed to [stats::quantile()].
#'
#' @keywords internal
getBootPercentiles <- function(
    .boot,
    .ci = 95,
    .percentiles = NULL,
    .na.rm = TRUE
){
  checkmate::assert_true(length(.ci) == 1)
  checkmate::assert_numeric(.percentiles, null.ok = TRUE)
  checkmate::assert_logical(.na.rm)

  comp_df <- .boot %>%
    dplyr::select(dplyr::starts_with(c("THETA", "SIGMA", "OMEGA")))

  quantile_fn <- function(x)  {
    stats::quantile(x, probs = .percentiles, na.rm = .na.rm)
  }

  # Convert .ci to percentiles when .percentiles is _not_ specified
  if (is.null(.percentiles)) {
    # Convert "IQR"
    .ci <- tolower(.ci)
    if (.ci == "iqr") .ci <- 50
    # Convert CI to percentiles
    .percentiles <- get_percentiles_from_ci(.ci)
  } else{
    # Perform checks and ensure order if .percentiles was specified
    if (any(.percentiles > 1) || any(.percentiles < 0)) {
      stop("`.percentiles` provided are outside of [0,1] range")
    }
    .percentiles <- unique(.percentiles[order(.percentiles)])
  }

  # Calculate quantiles for each column
  comp_df <- comp_df %>%
    dplyr::reframe(
      dplyr::across(.cols = dplyr::everything(), .fns = quantile_fn)
    ) %>% t() %>% as.data.frame() %>%
    tibble::rownames_to_column() %>%
    tibble::as_tibble()

  .percentiles_nam <- paste0("boot_perc_", .percentiles*100)
  colnames(comp_df) <- c("parameter_names", .percentiles_nam)

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
