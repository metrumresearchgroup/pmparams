#' @name pmparams
#' @title pmparams
#' @description `pmparams` is a library written in R that generates clear,
#' well-formatted parameter tables to report NONMEM model results.
#'
#' @details
#' `pmparams` creates parameter tables by leveraging a few intuitive functions designed to:
#'
#'* Read in your model parameters from various NONMEM output files
#'* Describe your model parameters in a YAML file
#'* Back-transform any parameters estimated in other domains (e.g., log- or logit-transformed variables)
#'* Calculate additional summary statistics (e.g., 95% confidence intervals, coefficient of variation (CV), etc.)
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data :=
#'
#' @keywords internal
globalVariables(
  c(
    "ci",
    "estimate",
    "fixed",
    "greekName",
    "lower",
    "name",
    "parameter_names",
    "trans",
    "transTHETA",
    "type_f",
    "upper",
    "value"
  )
)
