#' Load parameter estimates
#'
#' @description
#' Loads parameter estimates by providing model path, bbr model summary, or data frame of parameter estimates
#'
#' @param .estimates One of the following:
#' - Path to a `bbr` model
#' - A `bbr` model. (output of `bbr::read_model()`)
#' - A `bbr` model summary object (output of `bbr::model_summary()`)
#' - A data frame of the parameter estimates (e.g., output of `bbr::param_estimates()`)
#'
#' @keywords internal
loadParamEstimates <- function(.estimates){

  if (inherits(.estimates, "data.frame")){
    if (!("parameter_names" %in% colnames(.estimates))) {
      stop("`parameter_names` is an expected column name, which was not found.")
    } else{
      .estimates <- .estimates
    }
  } else{
    # If a dataframe is not provided, we rely on bbr to determine the estimates
    stop_if_missing_deps("bbr") # Check that bbr is installed

    if (inherits(.estimates, "character")){
      .estimates <- bbr::read_model(.estimates)
    }

    # model_summary requires bbi to have been set up, but we can let bbr handle that error
    if (inherits(.estimates, "bbi_nonmem_model")){
      .estimates <- bbr::model_summary(.estimates)
    }

    if (inherits(.estimates, "bbi_nonmem_summary")){
      .estimates <- bbr::param_estimates(.estimates)
    }else{
      # This gets triggered if _something other than a data frame_ was provided,
      # _and_ bbr was unable to follow the above procedure
      stop("Incorrect estimate input type")
    }
  }

  return(tibble::as_tibble(.estimates))
}

