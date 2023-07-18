#' @keywords internal
getpCV <- function(.df){
  .df %>%
    dplyr::mutate(
      cv = dplyr::case_when(
        diag & OM & lognormO ~ pmtables::sig(getCV_lognormO(value)),
        diag & S & propErr ~ pmtables::sig(getCV_propS(value)),
        TRUE ~ "-")
      ) %>%
    suppressWarnings()
}
