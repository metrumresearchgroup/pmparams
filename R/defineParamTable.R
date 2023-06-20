#' Combine parameter estimates with parameter key
#'
#' @description
#'
#' Combines model output parameter estimates with information in parameter key. Performs
#' some formatting of this combined data.frame.
#'
#' Expected input is a data.frame with parameter estimates, with the columns:
#' `parameter_names`, `estimate`, `stderr`, `random_effect_sd`, `random_effect_sdse`,
#' `fixed`, `diag`, `shrinkage`.
#'
#' Some `parameter_names` have punctuation such as `OMEGA(1,1)`. A new column is
#' added without punctuation, such as `OMEGA11`.
#'
#' Following this, parameter details from the parameter key are joined to the parameter estimates.
#' A `dplyr::inner_join` is used so that only parameters in the model output are kept
#' in the table. This was done so that, if your base and final model used the same structural
#' THETAs and random parameters, the same parameter key could be used for both.
#'
#' This join adds the following columns: `abb` (abbreviation), `desc` (parameter description),
#' `panel`, `trans` (transformation).
#'
#' With this information provided, a check is performed to determine whether parameters
#' with special transformation rules were defined correctly. In addition, a series of
#' TRUE/FALSE columns that will be used by subsequent functions.
#'
#' @param .estimates path to model directory, bbr NONMEM model, or data.frame of parameter estimates
#' @param .key path to parameter key or data.frame of parameter key
#' @param .ci confidence interval (default 95%)
#' @param .zscore z-score for the specified confidence interval. Only needed for confidence intervals that are NOT 90 or 95 percent
#'
#' @seealso \link[mrgparamtab]{param_key}: Parameter key requirements
#'
#' @example
#' paramPath <- system.file("model/nonmem/102", package = "mrgparamtab")
#' paramModel <- bbr::read_model(system.file("model/nonmem/102", package = "mrgparamtab"))
#' paramEst <- utils::read.csv(system.file("model/nonmem/param_est.csv", package = "mrgparamtab"))
#'
#' The following three commands should all render identical data.frames:
#' defineParamTable(.estimates = paramPath, .key = paramKey, .ci = 95, .zscore = NULL)
#' defineParamTable(.estimates = paramModel, .key = paramKey, .ci = 95, .zscore = NULL)
#' defineParamTable(.estimates = paramEst, .key = paramKey, .ci = 95, .zscore = NULL)
#'
#' To choose a confidence interval that is not 95 or 90, look up z-score and add as function parameter
#' defineParamTable(.estimates = paramEst, .key = paramKey, .ci = 82, .zscore = 0.915)
#'
#' @export
defineParamTable <- function(.estimates, .key, .ci = 95, .zscore = NULL){


  .estimates <- loadParamEstimates(.estimates)

  .key <- loadParamKey(.key)

  mod_estimates <- .estimates %>%
    removePunc(.column = "parameter_names") %>%
    dplyr::inner_join(.key, by = "name") %>%
    checkTransforms() %>%
    defineRows() %>%
    getValueSE() %>%
    getCI(.ci = .ci, .zscore = .zscore)

  return(mod_estimates)
}
