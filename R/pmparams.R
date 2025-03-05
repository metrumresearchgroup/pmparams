#' @name pmparams
#' @title pmparams
#' @description `pmparams` is a library written in R that generates clear,
#' well-formatted parameter tables to report NONMEM model results.
#'
#' @details `pmparams` creates parameter tables by leveraging a few intuitive
#' functions designed to:
#'
#'* Describe your model parameters in a YAML file
#'* Read in your model parameters from various NONMEM output files
#'* Back-transform any parameters estimated in other domains (e.g., log- or
#'  logit-transformed variables)
#'* Calculate additional summary statistics (e.g., 95% confidence intervals,
#'  coefficient of variation (CV), etc.)
#'
#' @seealso \code{\link[pmparams]{param_key}} example parameter key yaml
#' @section Key functions:
#'* \link[pmparams]{param_key} links to an example parameter key
#'* [define_param_table()] and [define_boot_table()]: combines and formats model
#'  output parameter estimates with information in parameter key.
#'* [format_param_table()] and [format_boot_table()]: format parameter estimate
#'  values and output selected columns to be shown in the parameter table
#'* [make_pmtable()] and [make_boot_pmtable()]: generate LaTeX-formatted
#'  parameter tables by filtering and using the `pmtables` package.
#'* [param_notes()] and [boot_notes()]: generate a list of generic footnote
#'  equations, abbreviations, and notes to append to parameter tables.
#'
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
    "value",
    "panel",
    "ci_level",
    "type",
    "greek",
    "shrinkage",
    "desc",
    "THETAERR"
  )
)
