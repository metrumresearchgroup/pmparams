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
#' @param .probs z-score for the specified confidence interval. Only needed for confidence intervals that are NOT 90 or 95 percent
#'
#' @keywords internal
getBootCI <- function(.boot_estimates, .ci = 95, .probs = NULL, .na.rm = FALSE) {

  comp_df <- .boot_estimates %>%
    dplyr::select(starts_with(c("THETA", "SIGMA", "OMEGA")))

  quantile_fn <- function(x)  {
    quantile(x, probs = probs, na.rm = .na.rm)
  }

  if (.ci == 95){
    .probs = c(.5, 0.025, 0.975)
  } else if (.ci == 90){
    .probs = c(.5, 0.05, 0.95)
  } else {
    .probs = .probs
  }

  ## error message if ci not 90 or 95
  ## error message if probs out of range


  .probs <- .probs[order(.probs)]

  comp_df <- comp_df %>%
    reframe(across(.cols = everything(), .fns = quantile_fn)) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column()

  colnames(comp_df) <- c("parameter_names", "value", "lower", "upper")


  comp_df_2 <- comp_df %>%
    mutate(
      boot_lower =.probs[1],
      boot_med = .probs[2],
      boot_upper = .probs[3]
    )

  return(comp_df_2)
}
