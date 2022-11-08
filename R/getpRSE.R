#' Calculate % RSE
#'
#' @description
#' Note, this is appropriate when parameters are estimated untransformed or in the log.
#' It may not be appropriate if any other transformations (such as logit) were used
#'
#' @param .df data.frame with parameter estimates
#' @param .value column name of value of interest
#' @param .se column name containing standard error
#' @param .digit set significant digits for output (optional)
#' @param .maxex set maxex for computation (optional)
#'
#' @export
getpRSE <- function(.df, .value = value, .se = se, ...,
                    .digit = getOption("mrgparamtab.dig"),
                    .maxex = getOption("mrgparamtab.maxex")){

  .digit = ifelse(is.null(.digit), formals(pmtables::sig)$digits, .digit)

  .df %>%
    dplyr::mutate(pRSE = dplyr::case_when(fixed ~ "-",
                                          TH & LOG ~ pmtables::sig(sqrt(exp({{.se}}^2)-1)*100, .digit, .maxex),
                                          TH & !LOG & !LOGIT ~ pmtables::sig(({{.se}}/abs({{.value}})) * 100, .digit, .maxex),
                                          diag & !LOG & !LOGIT ~ pmtables::sig(({{.se}}/abs({{.value}})) * 100, .digit, .maxex),
                                          TRUE ~ "-"))
  }

