#' Load parameter estimates
#'
#' @description
#' Loads parameter estimates by providing model path, bbr model summary, or data frame of parameter estimates
#'
#' @param .estimates path to model directory, bbr NONMEM model, or data.frame of parameter estimates
#'
#' @keywords internal
loadParamEstimates <- function(.estimates){

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
  } else{
    .estimates <- .estimates
  }
} else{
  stop("Incorrect estimate input type")
}

  .estimates
}
