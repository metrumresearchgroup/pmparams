#' Define how values are to be displayed
#'
#' @description
#' Format values for bootstrap run
#'
#' @keywords internal

formatValuesBoot <- function(.df){
  .df %>%
    dplyr::mutate(
      boot_ci = dplyr::if_else(fixed, "FIXED", paste0(pmtables::sig(lower), ', ', pmtables::sig(upper))),
      boot_value = pmtables::sig(value)
    )
}
