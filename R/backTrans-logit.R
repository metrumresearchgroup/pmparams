#' Back transform parameters estimated in the logit domain
#'
#' @description
#' Make sure any other calculations, such as CI (and pRSE) are
#' done before back-calculating these values.
#'
#' @keywords internal
backTrans_logit <- function(.df){
  .df %>%
    dplyr::mutate(
      value = dplyr::case_when(LOGIT ~ stats::plogis(value), TRUE ~ value),
      lower = dplyr::case_when(LOGIT ~ stats::plogis(lower), TRUE ~ lower),
      upper = dplyr::case_when(LOGIT ~ stats::plogis(upper), TRUE ~ upper)
      )
}
