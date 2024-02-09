#' Define a series of true/false columns
#'
#' @description
#'
#' Creates variables with true/false values depending on the type of parameter
#' on each row. For instance, an "OMEGA" parameter would be OM ~ TRUE and TH ~ FALSE & S ~ FALSE.
#'
#' Similarly, true/false variables are created to indicate the transform selected.
#' @param .df data frame containing parameter estimates & names
#'
#'
#' @keywords internal
defineRows <- function(.df){

  transList <- c("logTrans", "logitTrans", "lognormalOm", "OmSD", "propErr", "addErr", "none")
  mismatch <- .df %>% dplyr::filter(!(stringr::str_detect(trans, paste(transList, collapse="|"))))

  if(nrow(mismatch) > 0) {
    print(mismatch)
    stop("Invalid transform value. See ?param_key for list of valid transform values") # link to what valid values are
  }
  .df %>%
    dplyr::mutate(
      THETAERR = stringr::str_detect(name, "^TH") & stringr::str_detect(panel, "RV"),
      TH = stringr::str_detect(name, "^TH") & !THETAERR,
      OM = stringr::str_detect(name, "^OM"),
      S = stringr::str_detect(name, "^S") |  THETAERR,
      LOG = (trans=="logTrans"),
      LOGIT = (trans=="logitTrans"),
      lognormO = (trans=="lognormalOm"),
      Osd = (trans=="OmSD"),
      logitOsd = (trans=="logitOmSD"),
      propErr = (trans=="propErr"),
      addErr = (trans=="addErr")
    )
}

