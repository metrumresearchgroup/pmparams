#' Format parameter table
#'
#' @description
#'
#' Format parameter estimate values and output selected columns to be shown in
#' the parameter table.
#'
#' @param .df parameter estimates with modifications ready for formatting
#' @param .select_cols columns to select for output
#'
#' @export
format_param_table <- function(.df,
                               .select_cols = c("type", "abb", "greek", "desc", "value", "ci", "shrinkage")){

 .df %>%
   formatValues() %>%
   formatGreekNames() %>%
   getPanelName() %>%
   dplyr::select(.select_cols)

}
