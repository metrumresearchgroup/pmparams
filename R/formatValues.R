#' Define how values are to be displayed
#'
#' @description
#' Define what is in estimate column and what is in square brackets.
#'
#' @keywords internal
formatValues <- function(.df,
                         .digit,
                         .maxex){

  .df %>%
    backTrans_log() %>%
    backTrans_logit() %>%
    getpCV() %>%
    getpRSE() %>%
    dplyr::mutate(
      ci = paste0(pmtables::sig(lower, .digit, .maxex), ', ', pmtables::sig(upper, .digit, .maxex)),
      ci = dplyr::if_else(fixed, "FIXED", ci),
      sd = dplyr::case_when(
        diag & OM & Osd ~ pmtables::sig(random_effect_sd, .digit, .maxex),
        diag & OM & logitOsd ~ pmtables::sig(getSD_logitO(.mean=transTHETA, .var = value), .digit, .maxex),
        TRUE ~ "-"
      ),
      corr_SD = dplyr::case_when(is.na(corr_SD) ~ "-", TRUE ~ pmtables::sig(corr_SD, .digit, .maxex)),
      value = pmtables::sig(value, .digit, .maxex),
      value = dplyr::case_when(
        diag & OM & Osd | diag & OM & logitOsd ~ glue::glue("{value} {parensSQ_se(sd)}"),
        diag & OM | diag & S & propErr ~ glue::glue("{value} {parensSQ_CV(cv)}"),
        diag & OM | diag & S & addErrLogDV ~ glue::glue("{value} {parensSQ_CV(cv)}"),
        !diag & OM ~ glue::glue("{value} {parensSQ_corr(corr_SD)}"),
        diag & S & addErr ~ glue::glue("{value} {parensSQ_se(corr_SD)}"),
        !diag & S ~ glue::glue("{value} {parensSQ_corr(corr_SD)}"),
        TRUE ~ value),
      shrinkage = dplyr::case_when(is.na(shrinkage) ~ "-", TRUE ~ pmtables::sig(shrinkage, .digit, .maxex))
    )
}
