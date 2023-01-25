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
#' @param .df parameter estimates with modifications ready for formatting
#' @param .select_cols columns to select for output. Default selects "type", "abb", "greek", "desc", "value", "ci", "shrinkage". To return all columns, specify "all" for .select_cols.
#' @param .prse output pRSE. Default is FALSE.
#' @export
formatParamTable <- function(.df,
                             .select_cols = c("type", "abb", "greek", "desc", "value", "ci", "shrinkage"),
                             .prse = FALSE){

all <- c("all", "All", "ALL")
if (.select_cols %in% all & .prse == FALSE){
  .df %>%
    formatValues() %>%
    formatGreekNames() %>%
    getPanelName() %>%
    dplyr::select(-pRSE)
} else if (.select_cols %in% all & .prse == TRUE){
  .df %>%
    formatValues() %>%
    formatGreekNames() %>%
    getPanelName()
} else if (!(.select_cols %in% all) & .prse == FALSE){
  .df %>%
    formatValues() %>%
    formatGreekNames() %>%
    getPanelName() %>%
    dplyr::select(all_of(.select_cols))
} else {
  .df %>%
    formatValues() %>%
    formatGreekNames() %>%
    getPanelName() %>%
    dplyr::select(all_of(append(.select_cols, "pRSE")))
}
}
