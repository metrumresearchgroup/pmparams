#' Remove punctuation from parameter names
#'
#' @description
#'
#' The model output for parameter estimates include punctuation in the parameter
#' names, such as `OMEGA(1,1)`. Often it is desirable to have the name as
#' `OMEGA11` instead.
#'
#' @keywords internal
removePunc <- function(.df, .column){
  modDF <- .df %>%
    dplyr::mutate(name = gsub("[[:punct:]]", "", .df[[.column]]))
  return(modDF)
  }
