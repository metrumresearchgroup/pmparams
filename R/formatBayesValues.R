#' Define how values are to be displayed
#'
#' @description
#' Define what is in estimate column and what is in square brackets.
#'
#' @keywords internal
formatBayesValues <- function(.df,
                         .digit,
                         .maxex){

  .df %>%
    backTrans_log(.col_list = c(.summary_stat, "lower", "upper")) %>%
    backTrans_logit(.col_list = c(.summary_stat, "lower", "upper")) %>%
    #getpCV() %>% #cannot calculate pCV the same way bc need to get pCV for each chain and then get mean?
    #getpRSE() %>%
    dplyr::mutate(
      ci = paste0(pmtables::sig(lower, .digit, .maxex), ', ', pmtables::sig(upper, .digit, .maxex)),
      ci = dplyr::if_else(fixed, "FIXED", ci),
      # sd = dplyr::case_when(
      #   diag & OM & Osd ~ pmtables::sig(random_effect_sd),
      #   diag & OM & logitOsd ~ pmtables::sig(getSD_logitO(.mean=transTHETA, .var = value)),
      #   TRUE ~ "-"
      # ),
      value = pmtables::sig(value, .digit, .maxex),
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
