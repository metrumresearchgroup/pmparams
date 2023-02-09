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
#' @param .zed z-score for the specified confidence interval. Only needed for confidence intervals that are NOT 90 or 95 percent
#' @examples
#'
#' Theoph_ex <- Theoph %>% dplyr::mutate(stderr = conc/10)
#' getCI(.df = Theoph_ex, .value = "conc", .se = "stderr", .ci = 95)
#'
#' getCI(.df = Theoph_ex, .value = "conc", .se = "stderr", .ci = 80, .zed = 1.282)
#'
#' @keywords internal
getCI <- function(.df, .value = "value", .se = "se", .ci = 95, .zed = NULL){

  .df <- grouped_warn_ungroup(.df, as.character(match.call()[[1]]))

  if (.ci %in% c(90, 95)){
    .df %>%
      dplyr::mutate(lower = lowerCI(.est = .df[[.value]], .se = .df[[.se]], .ci = .ci),
                    upper = upperCI(.est = .df[[.value]], .se = .df[[.se]], .ci = .ci))
  } else if (!is.null(.zed)){
    .df %>%
      dplyr::mutate(lower = lowerCI(.est = .df[[.value]], .se = .df[[.se]], .ci = .ci, .zed = .zed),
                    upper = upperCI(.est = .df[[.value]], .se = .df[[.se]], .ci = .ci, .zed = .zed))
  } else if (!(.ci %in% c(90, 95)) & is.null(.zed)){
    stop("Z-score (.zed) must be supplied when CI is not 90 or 95")
  }

}
