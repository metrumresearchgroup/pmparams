#' Back transform parameters estimated in the log domain
#'
#' @description
#' Make sure any other calculations, such as CI (and pRSE) are
#' done before back-calculating these values.
#'
#' @export
backTrans_log <- function(df){
  df %>%
    mutate(value = case_when(LOG ~ exp(value), TRUE ~ value),
           lower = case_when(LOG ~ exp(lower), TRUE ~ lower),
           upper = case_when(LOG ~ exp(upper), TRUE ~ upper))
}
