#' Define how values are to be displayed
#'
#' @description
#' Define what is in estimate column and what is in square brackets.
#'
#' @keywords internal
formatValues <- function(.df,
                         .digit = getOption("mrgparamtab.dig"),
                         .maxex = getOption("mrgparamtab.maxex")){

  .digit = ifelse(is.null(.digit), formals(pmtables::sig)$digits, .digit)

  .df %>%
    backTrans_log() %>%
    backTrans_logit() %>%
    getpCV() %>%
    dplyr::mutate(
      ci = paste0(pmtables::sig(.df$lower, .digit, .maxex), ', ', pmtables::sig(.df$upper, .digit, .maxex)),
      ci = dplyr::if_else(.df$fixed, "FIXED", ci),
      sd = dplyr::case_when(
        diag & OM & Osd ~ pmtables::sig(random_effect_sd),
        diag & OM & logitOsd ~ pmtables::sig(getSD_logitO(.mean=transTHETA, .var = value)),
        TRUE ~ "-"
        ),
      value = pmtables::sig(.df$value, .digit, .maxex),
      value = dplyr::case_when(
        diag & OM & Osd | diag & OM & logitOsd ~ glue::glue("{value} {parensSQ_se(sd)}"),
        diag & OM | diag & S & propErr ~ glue::glue("{value} {parensSQ_CV(cv)}"),
        !diag & OM ~ glue::glue("{value} {parensSQ_corr(corr_SD)}"),
        diag & S & addErr ~ glue::glue("{value} {parensSQ_se(corr_SD)}"),
        !diag & S ~ glue::glue("{value} {parensSQ_corr(corr_SD)}"),
        TRUE ~ value),
      shrinkage = dplyr::case_when(is.na(shrinkage) ~ "-", TRUE ~ pmtables::sig(shrinkage))
      )
}
