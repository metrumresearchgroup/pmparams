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
  mismatch <- .df %>% dplyr::filter(!(stringr::str_detect(.df$trans, paste(transList, collapse="|"))))

  if(nrow(mismatch) > 0) {
    print(mismatch)
    stop("Invalid transform value. See ?param_key for list of valid transform values") # link to what valid values are
  }
  .df %>%
    dplyr::mutate(
      TH = stringr::str_detect(.df$name, "^TH"),
      OM = stringr::str_detect(.df$name, "^OM"),
      S = stringr::str_detect(.df$name, "^S"),
      LOG = (.df$trans=="logTrans"),
      LOGIT = (.df$trans=="logitTrans"),
      lognormO = (.df$trans=="lognormalOm"),
      Osd = (.df$trans=="OmSD"),
      logitOsd = (.df$trans=="logitOmSD"),
      propErr = (.df$trans=="propErr"),
      addErr = (.df$trans=="addErr")
    )
}

