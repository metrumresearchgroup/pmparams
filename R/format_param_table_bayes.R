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

  if (!any(names(.df) %in% c("software"))){
    stop("Software column required")
  }

  if (length(unique(.df$software)) > 1){
    stop("Only 1 software type allowed")
  }

  ci_col <- .df[grepl("q",colnames(.df))]

  ci_col1 <- ci_col %>%
    tidyr::pivot_longer(everything())  %>%
    dplyr::distinct(name) %>%
    dplyr::mutate(
      num_name = as.numeric(gsub("q", "", name)),
      new_name = if_else(num_name == min(num_name), "low", "high")
    ) %>%
    dplyr::select(name, new_name) %>%
    tidyr::pivot_wider(names_from = new_name, values_from = name)


  .df1 <- .df %>%
    dplyr::rename(low = ci_col1$low, high = ci_col1$high) %>%
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
