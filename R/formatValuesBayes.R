#' Define how values are to be displayed
#'
#' @description
#' Define what is in estimate column and what is in square brackets.
#'
#' @keywords internal
formatValuesBayes <- function(.df,
                         .digit,
                         .maxex){

  # .df <- define_param_table_bayes(.estimates = fit0, .key = bayes_key)
  # .select_cols = "all"
  # .digit = getOption("pmparams.dig")
  # .maxex = getOption("pmparams.maxex")
  # .digit = ifelse(is.null(.digit), formals(pmtables::sig)$digits, .digit)

  .df1 <- .df %>%
    #QC: add code to determine if fixed/diag. In bbr, when load in model get these cols. Adapt for Bayes
    dplyr::mutate(fixed = FALSE,
           diag = NA_real_) %>%
    backTrans_log(.col_list = c(.summary_stat, "lower", "upper")) %>%
    backTrans_logit(.col_list = c(.summary_stat, "lower", "upper")) %>%
    #getpCV() %>% #cannot calculate pCV the same way bc need to get pCV for each chain and then get mean?
    #getpRSE() %>%
    dplyr::mutate(
      ci = paste0(pmtables::sig(lower, .digit, .maxex), ', ', pmtables::sig(upper, .digit, .maxex)),
      ci = dplyr::if_else(fixed, "FIXED", ci)
    )

  .df1
#not running transformations for now/
#   if (any(names(.df1) %in% c("sd"))){
#     .df2 <- .df1 %>%
#       dplyr::mutate(
#         sd = dplyr::case_when(
#           diag & OM & Osd ~ pmtables::sig(sd),
#           diag & OM & logitOsd ~ pmtables::sig(getSD_logitO(.mean=transTHETA, .var = median)), #avlue = median
#           TRUE ~ "-"
#         )
#       )
#   } else {
#     .df2 <- .df2
#   }
#
#
#   for (names(.df1) %in% c("median", "mean", "mad")){
#       value = pmtables::sig(value, .digit, .maxex),
#       value = dplyr::case_when(
#         diag & OM & Osd | diag & OM & logitOsd ~ glue::glue("{value} {parensSQ_se(sd)}"),
#         diag & OM | diag & S & propErr ~ glue::glue("{value} {parensSQ_CV(cv)}"),
#         !diag & OM ~ glue::glue("{value} {parensSQ_corr(corr_SD)}"),
#         diag & S & addErr ~ glue::glue("{value} {parensSQ_se(corr_SD)}"),
#         !diag & S ~ glue::glue("{value} {parensSQ_corr(corr_SD)}"),
#         TRUE ~ value),
#       shrinkage = dplyr::case_when(is.na(shrinkage) ~ "-", TRUE ~ pmtables::sig(shrinkage))
#     )
# }

  }
