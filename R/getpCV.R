#' @keywords internal
getpCV <- function(.df,
                   .digit = getOption("pmparams.dig"),
                   .maxex = getOption("pmparams.maxex")){

  .digit = ifelse(is.null(.digit), formals(pmtables::sig)$digits, .digit)

  .df %>%
    dplyr::mutate(
      cv = dplyr::case_when(
        diag & OM & lognormO ~ pmtables::sig(getCV_lognormO(value), .digit, .maxex),
        diag & S & propErr ~ pmtables::sig(getCV_propS(value), .digit, .maxex),
        diag & S & addErrLogDV ~ pmtables::sig(getCV_lognormO(value), .digit, .maxex),
        TRUE ~ "-")
      ) %>%
    suppressWarnings()
}
