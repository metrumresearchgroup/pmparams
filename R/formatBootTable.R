#' Format parameter table
#'
#' @description
#'
#' Format parameter estimate values and output selected columns to be shown in
#' the parameter table.
#'
#' There are four main steps of this function:
#' 1. When necessary, back transform parameters and their CIs, round parameters using pmtables::sig, and combine columns.
#' 2. Format the THETA/OMEGA/SIGMA values to display as greek letters in Latex, with subscript numbers, and where necessary, the transformation applied to that parameter.
#' 3. Determine which panel of the final table the parameter should be displayed in. This is informed by the panel argument you defined in your parameter key. Note that there are a finite number of options included by default (see below) but you can include additional panels as needed.
#' Panel types include:
#' - Residual variance
#' - Interindividual covariance parameters
#' - Interindividual variance parameters
#' - Interoccasion variance parameters
#' - Covariate effect parameters
#' - Structural model parameters
#' 4. Select columns for final tables.
#'
#'
#' @param .boot_df boot parameter estimates with modifications ready for formatting
#' @param .param_df parameter estimates with modifications ready for formatting
#' @param .digit set significant digits for output (optional)
#' @param .maxex set maxex for computation (optional)
#'
#' @export
formatBootTable <- function(.boot_df,
                            .param_df,
                           #  .select_cols = c("type", "abb", "greek", "desc", "value", "ci", "shrinkage"),
                            # .prse = FALSE,
                             .digit = getOption("mrgparamtab.dig"),
                             .maxex = getOption("mrgparamtab.maxex")){

  .digit = ifelse(is.null(.digit), formals(pmtables::sig)$digits, .digit)

  # if (.prse == TRUE) {
  #   .df <- .df %>% getpRSE()
  #   .select_cols <- append(.select_cols, "pRSE")
  # }

  .bootParam = dplyr::left_join(.param_df, .boot_df, by = c("abb", "desc"))

  .boot_estimates <- .bootParam %>%
    removePunc(.column = "parameter_names") %>%
    dplyr::inner_join(.key, by = "name") %>%
    checkTransforms() %>%
    defineRows() %>%
    getValueSE() %>%
    getCI()

  return(.boot_estimates)

  # if (any(tolower(.select_cols) == "all")) {
  #   return(.df_out %>% as.data.frame())
  # } else {
  #   return(.df_out %>%
  #            dplyr::select(.select_cols) %>%
  #            as.data.frame())
  # }

}


