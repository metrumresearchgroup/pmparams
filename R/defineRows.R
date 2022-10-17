#' Define a series of true/false columns
#'
#' @description
#'
#' Creates variables with true/false values depending on the type of parameter
#' on each row. For instance, an "OMEGA" parameter would be OM ~ TRUE and TH ~ FALSE & S ~ FALSE.
#'
#' Similarly, true/false variables are created to indicate the transform selected.
#'
#' @param .df data frame containing parameter estimataes & names
#'
#' @keywords internal
defineRows <- function(.df){
  .df %>%
    dplyr::mutate(
      TH = stringr::str_detect(name, "TH"),
      OM = stringr::str_detect(name, "OM"),
      S = stringr::str_detect(name, "S"),
      LOG = (trans=="logTrans"),
      LOGIT = (trans=="logitTrans"),
      lognormO = (trans=="lognormalOm"),
      Osd = (trans=="OmSD"),
      logitOsd = (trans=="logitOmSD"),
      propErr = (trans=="propErr"),
      addErr = (trans=="addErr")
    )
}
