#' Define how values are to be displayed
#'
#' @description
#' Format values for bootstrap run
#'
#' @keywords internal

formatValuesBoot <- function(.df,
                             .digit,
                             .maxex){
  .df %>%
    dplyr::mutate(
      boot_ci_95 = dplyr::if_else(fixed, "FIXED", paste0(pmtables::sig(lower, .digit, .maxex), ', ', pmtables::sig(upper, .digit, .maxex))),
      boot_value = pmtables::sig(value, .digit, .maxex)
    )
    )
}
