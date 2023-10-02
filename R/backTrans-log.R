#' Back transform parameters estimated in the log domain
#'
#' @description
#' Make sure any other calculations, such as CI (and pRSE) are
#' done before back-calculating these values.
#'
#' @keywords internal
backTrans_log <- function(.df){
  .df %>%
    dplyr::mutate(
      value = dplyr::case_when(LOG ~ exp(value), TRUE ~ value),
      lower = dplyr::case_when(LOG ~ exp(lower), TRUE ~ lower),
      upper = dplyr::case_when(LOG ~ exp(upper), TRUE ~ upper)
      )
}
