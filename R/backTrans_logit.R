#' Back transform parameters estimated in the logit domain
#'
#' @description
#' Make sure any other calculations, such as CI (and pRSE) are
#' done before back-calculating these values.
#'
#' @export
backTrans_logit <- function(df){
  df %>%
    mutate(value = case_when(LOGIT ~ exp(value)/(1+exp(value)), TRUE ~ value),
           lower = case_when(LOGIT ~ exp(lower)/(1+exp(lower)), TRUE ~ lower),
           upper = case_when(LOGIT ~ exp(upper)/(1+exp(upper)), TRUE ~ upper))
}
