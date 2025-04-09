#' Define how values are to be displayed
#'
#' @description
#' Define what is in estimate column and what is in square brackets.
#'
#' @param .df Parameter estimates output from [define_param_table()] with
#'   modifications ready for formatting.
#' @inheritParams display_value
#' @keywords internal
formatValues <- function(.df,
                         .digit,
                         .maxex){

  .df %>%
    backTrans_log() %>%
    backTrans_logit() %>%
    getpCV() %>%
    dplyr::mutate(
      ci = paste0(display_value(lower, .digit, .maxex), ', ', display_value(upper, .digit, .maxex)),
      ci = dplyr::if_else(fixed, "FIXED", ci),
      sd = dplyr::case_when(
        diag & OM & Osd ~ display_value(random_effect_sd, .digit, .maxex),
        diag & OM & logitOsd ~ display_value(getSD_logitO(.mean=transTHETA, .var = value), .digit, .maxex),
        TRUE ~ "-"
      ),
      corr_SD = dplyr::case_when(is.na(corr_SD) ~ "-", TRUE ~ display_value(corr_SD, .digit, .maxex)),
      value = display_value(value, .digit, .maxex),
      value = dplyr::case_when(
        diag & OM & Osd | diag & OM & logitOsd ~ glue::glue("{value} {parensSQ_se(sd)}"),
        diag & OM | diag & S & propErr ~ glue::glue("{value} {parensSQ_CV(cv)}"),
        diag & OM | diag & S & addErrLogDV ~ glue::glue("{value} {parensSQ_CV(cv)}"),
        !diag & OM ~ glue::glue("{value} {parensSQ_corr(corr_SD)}"),
        diag & S & addErr ~ glue::glue("{value} {parensSQ_se(corr_SD)}"),
        !diag & S ~ glue::glue("{value} {parensSQ_corr(corr_SD)}"),
        TRUE ~ value),
      shrinkage = dplyr::case_when(is.na(shrinkage) ~ "-", TRUE ~ display_value(shrinkage, .digit, .maxex))
    ) %>% suppressSpecificWarning("NAs introduced by coercion")
}


#' Format values for display purposes
#'
#' Format significant digits and substitute negative signs (`-`) for LaTeX
#' styling (`$-$`)
#'
#' @param x Numeric; value to manipulate
#' @param .digit Number of significant digits. Default is three digits
#' @param .maxex Maximum number of significant digits before moving to scientific
#'   notation. Default is `NULL`
#'
#' @keywords internal
display_value <- function(x, .digit = NULL, .maxex = NULL){
  .digit <- ifelse(is.null(.digit), formals(pmtables::sig)$digits, .digit)
  pmtables::sig(x, .digit, .maxex) %>%
    format_negative_vals()
}

#' Substitute negative signs (`-`) for LaTeX styling (`$-$`)
#' @noRd
format_negative_vals <- function(x){
  sub("^-", "$-$", x)
}
