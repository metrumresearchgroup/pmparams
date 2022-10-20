#' Calculate % RSE
#'
#' @description
#' Note, this is appropriate when parameters are estimated untransformed or in the log.
#' It may not be appropriate if any other transformations (such as logit) were used
#'
#' @export
getpRSE <- function(.df, .digit = getOption("mrgparamtab.dig"), .maxex = getOption("mrgparamtab.maxex")){

  if(is.null(.digit) & is.null(.maxex)){
    .df %>%
      dplyr::mutate(pRSE = dplyr::case_when(fixed ~ "-",
                                            TH & LOG ~ pmtables::sig(sqrt(exp(se^2)-1)*100),
                                            TH & !LOG & !LOGIT ~ pmtables::sig((se/abs(value)) * 100),
                                            diag & !LOG & !LOGIT ~ pmtables::sig((se/abs(value)) * 100),
                                            TRUE ~ "-"))
  } else{
    .df %>%
      dplyr::mutate(pRSE = dplyr::case_when(fixed ~ "-",
                                            TH & LOG ~ pmtables::sig(sqrt(exp(se^2)-1)*100, .digit, .maxex),
                                            TH & !LOG & !LOGIT ~ pmtables::sig((se/abs(value)) * 100, .digit, .maxex),
                                            diag & !LOG & !LOGIT ~ pmtables::sig((se/abs(value)) * 100, .digit, .maxex),
                                            TRUE ~ "-"))
  }

}
