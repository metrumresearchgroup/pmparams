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
#' @param .df parameter estimates output from `defineParamTable` with modifications ready for formatting
#' @param .select_cols columns to select for output. Default selects "type", "abb", "greek", "desc", "value", "ci", "shrinkage". To return all columns, specify "all" for .select_cols
#' @param .prse output pRSE. Default is FALSE
#' @param .digit set significant digits for output (optional). Default is three digits
#' @param .maxex set maxex for computation (optional). Default is NULL
#'
#' @examples
#'
#' #Using output from `defineParamTable` (defineOut),
#' paramEst <- utils::read.csv(system.file("model/nonmem/param_est.csv", package = "mrgparamtab"))
#' paramKey <-  system.file("model/nonmem/pk-parameter-key-new.yaml", package = "mrgparamtab")
#' defineOut <- defineParamTable(.estimates = paramEst, .key = paramKey, .ci = 95, .zscore = NULL)
#'
#' formatParamTable(.df = defineOut)
#'
#' #To include all columns:
#'
#' formatParamTable(.df = defineOut, .select_cols="all")
#' @export
formatParamTable <- function(.df,
                             .select_cols = c("type", "abb", "greek", "desc", "value", "ci", "shrinkage"),
                             .prse = FALSE,
                             .digit = getOption("mrgparamtab.dig"),
                             .maxex = getOption("mrgparamtab.maxex")){

  .digit = ifelse(is.null(.digit), formals(pmtables::sig)$digits, .digit)

  if (.prse == TRUE) {
    .df <- .df %>% getpRSE()
    .select_cols <- append(.select_cols, "pRSE")
  }

  .df_out <-
    .df %>%
    formatValues(.digit = .digit, .maxex = .maxex) %>%
    formatGreekNames() %>%
    getPanelName()

  if (any(tolower(.select_cols) == "all")) {
    return(.df_out %>% as.data.frame())
  } else {
    return(.df_out %>%
      dplyr::select(.select_cols) %>%
      as.data.frame())
  }

}
