#' Combine parameter estimates with parameter key
#'
#' @description
#'
#' Combines model output parameter estimates with information in parameter key. Performs
#' some formatting of this combined data.frame.
#'
#' Expected input is parameter key yaml and:
#' - Path to model OR
#' - Data.frame with parameter estimates, with the columns:
#' `parameter_names`, `estimate`, `stderr`, `random_effect_sd`, `random_effect_sdse`,
#' `fixed`, `diag`, `shrinkage`.
#'
#' This function does the following:
#' 1. Remove punctuation from columns, as needed. Some `parameter_names` have punctuation such as `OMEGA(1,1)`. A new column is
#' added without punctuation, such as `OMEGA11`.
#'
#' 2. Join parameter details from the parameter key to the parameter estimates.
#' A `dplyr::inner_join` is used so that only parameters in the model output are kept
#' in the table. This was done so that, if your base and final model used the same structural
#' THETAs and random parameters, the same parameter key could be used for both.
#'
#' This join adds the following columns: `abb` (abbreviation), `desc` (parameter description),
#' `panel`, `trans` (transformation).
#'
#' 3. Perform check to determine whether parameters with special transformation rules were defined correctly. In addition, a series of
#' TRUE/FALSE columns that will be used by subsequent functions.
#'
#' 4. Calculate upper and lower bound of a defined confidence interval, based on the
#' value and standard error.
#'
#' @param .estimates parameter estimates data.frame or path to model
#' @param .key parameter key
#' @param .ci specify confidence interval (default 95%)
#' @param .zed z-score for the specified confidence interval. Only needed for confidence intervals that are NOT 90 or 95 percent
#'
#' @seealso \link[mrgparamtab]{param_key}: Parameter key requirements
#' @export
defineParamTable <- function(.estimates, .key, .ci = 95, .zed = NULL){


  if (inherits(.estimates, "character")){
    print(paste0("Model path provided: ", .estimates))
    .estimates <- bbr::read_model(.estimates)
  }

  if (inherits(.estimates, "bbi_nonmem_model")){
    .estimates <- bbr::model_summary(.estimates)
  }

  if (inherits(.estimates, "bbi_nonmem_summary")){
    .estimates <- bbr::param_estimates(.estimates)
  }

  if (inherits(.estimates, "data.frame")){
    if (!("parameter_names" %in% colnames(.estimates))) {
      stop("Incorrect estimate input type")
    }
  } else{
    stop("Incorrect estimate input type")
  }

  if (inherits(.key, "character")){
    print(paste0("Parameter table yaml path provided: ", .key))
    y1l <- yaml::yaml.load_file(.key)

    if (!all(names(y1l[[1]]) %in% c("abb", "desc", "panel", "trans"))) {
      warning("Only abb, desc, panel and trans arguments will be used, all others ignored")
    }

    .key <- dplyr::tibble(
      name = names(y1l),
      abb = unlist(y1l)[grepl('abb',names(unlist(y1l)),fixed=T)],
      desc = unlist(y1l)[grepl('desc',names(unlist(y1l)),fixed=T)],
      panel = unlist(y1l)[grepl('panel',names(unlist(y1l)),fixed=T)],
      trans = unlist(y1l)[grepl('trans',names(unlist(y1l)),fixed=T)]
    )
  }

  if (inherits(.key, "data.frame")){
    if (!(all(c("name", "abb", "desc", "panel", "trans") %in% colnames(.key)))) {
      stop("Incorrect parameter key input type. See ?param_key for list of valid parameter key inputs")
    }
  } else{
    stop("Incorrect parameter key input type. See ?param_key for list of valid parameter key inputs")
  }

  mod_estimates <- .estimates %>%
    removePunc(.column = "parameter_names") %>%
    dplyr::inner_join(.key, by = "name") %>%
    checkTransforms() %>%
    defineRows() %>%
    getValueSE() %>%
    getCI(.ci = .ci, .zed = .zed)

  return(mod_estimates)
}
