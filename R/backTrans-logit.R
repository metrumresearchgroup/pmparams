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
      value = dplyr::case_when(LOGIT ~ exp(value)/(1+exp(value)), TRUE ~ value),
      lower = dplyr::case_when(LOGIT ~ exp(lower)/(1+exp(lower)), TRUE ~ lower),
      upper = dplyr::case_when(LOGIT ~ exp(upper)/(1+exp(upper)), TRUE ~ upper)
      )
}
