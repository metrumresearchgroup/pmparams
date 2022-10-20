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
#' @param .estimates parameter estimates data.frame
#' @param .key parameter key
#' @param ... arguments passed through to methods (currently undefined)
#'
#' @seealso \link[mrgparamtab]{param_key}: Parameter key requirements
#' @export
define_param_table <- function(.estimates, .key){

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
    print(paste0("Model path provided: ", .key))
    .key <- pmtables::yaml_as_df(.key)
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
    defineRows()

  return(mod_estimates)
}
