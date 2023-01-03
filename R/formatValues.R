#' Define how values are to be displayed
#'
#' @description
#' Define what is in estimate column and what is in square brackets.
#'
#' Displays 95% CI to show lower, upper or FIXED and rounds values for display in report
#'
#' @keywords internal
formatValues <- function(df){
  df %>%
    # back transform any parameters here
    backTrans_log() %>%   # back transform from log domain
    backTrans_logit() %>%
    getpCV() %>%          # get % CV
    # format the values for the final table
    mutate(ci = paste0(sig(lower), ', ', sig(upper)),
           ci = if_else(fixed, "FIXED", ci),
           # get sd if needed
           sd = case_when(diag & OM & Osd ~ sig(random_effect_sd),
                          diag & OM & logitOsd ~ sig(getSD_logitO(.mean=transTHETA, .var = value)),
                          TRUE ~ "-"
           ),
           # round values for report table
           value = sig(value),
           # define which values appear where
           value = case_when(diag & OM & Osd |
                               diag & OM & logitOsd ~ glue::glue("{value} {parensSQ_se(sd)}"),

                             diag & OM |
                               diag & S & propErr ~
                               glue::glue("{value} {parensSQ_CV(cv)}"),
                             !diag & OM ~ glue::glue("{value} {parensSQ_corr(corr_SD)}"),
                             diag & S & addErr ~ glue::glue("{value} {parensSQ_se(corr_SD)}"),
                             !diag & S ~ glue::glue("{value} {parensSQ_corr(corr_SD)}"),
                             TRUE ~ value),
           # round shrinkage values for report table
           shrinkage = case_when(is.na(shrinkage) ~ "-",
                                 TRUE ~ sig(shrinkage)))

}
