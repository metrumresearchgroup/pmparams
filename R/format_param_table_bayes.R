#' Format parameter table for bayes model
#'
#' @description
#'
#' Format parameter estimate values and output selected columns to be shown in
#' the parameter table.
#'
#'
#' @param .df parameter estimates output from `define_param_table_bayes` with modifications ready for formatting
#' @param .select_cols columns to select for output. Default selects all columns.
#' @param .digit set significant digits for output (optional). Default is three digits
#' @param .maxex set maxex for computation (optional). Default is NULL
#'
#' @export
format_param_table <- function(.df,
                             .select_cols = "all",
                             .digit = getOption("pmparams.dig"),
                             .maxex = getOption("pmparams.maxex")){

  .df <- define_param_table_bayes(.estimates = fit0, .key = bayes_key)
  .select_cols = "all"
  .digit = getOption("pmparams.dig")
  .maxex = getOption("pmparams.maxex")

  .digit = ifelse(is.null(.digit), formals(pmtables::sig)$digits, .digit)




  .df1 <- .df %>%
    dplyr::mutate(
        ci = paste0(pmtables::sig(low, .digit, .maxex), ', ', pmtables::sig(high, .digit, .maxex))
    ) %>%
    select(-low, -high)

  if (unique(.df$software) == "nonmem"){
    #Add here transformations- similar to regular parameter table transformations?
    .df_out <- .df1 #%>%
      #formatValues(.digit = .digit, .maxex = .maxex, .model_type = "bayes")
  } else {
    .df_out <- .df1
  }


  if (any(tolower(.select_cols) == "all")) {
    return(.df_out %>% as.data.frame())
  } else {
    return(.df_out %>%
      dplyr::select(.select_cols) %>%
      as.data.frame())
  }

}
