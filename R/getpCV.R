#' Calculate CV%
#'
#' @param .df data.frame with combined parameter estimates and parameter key
#' @param .column column name to perform calculation on
#'
#' @export
getpCV <- function(.df, .column = "value"){
  .df %>%
    dplyr::mutate(cv = dplyr::case_when(
      diag & OM & lognormO ~ pmtables::sig(getCV_lognormO(.df[[.column]])),
      diag & S & propErr ~ pmtables::sig(getCV_propS(.df[[.column]])),
      TRUE ~ "-"))
}
