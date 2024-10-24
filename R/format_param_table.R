#' Format parameter table
#'
#' @description
#'
#' Format parameter estimate values and output selected columns to be shown in
#' the parameter table.
#'
#' There are four main steps of this function:
#'1. When necessary, back transform parameters and their CIs, round parameters using pmtables::sig, and combine columns.
#'2. Format the THETA/OMEGA/SIGMA values to display as greek letters in Latex, with subscript numbers, and where necessary, the transformation applied to that parameter.
#'3. Determine which panel of the final table the parameter should be displayed in. This is informed by the panel argument you defined in your parameter key. Note that there are a finite number of options included by default (see below) but you can include additional panels as needed.
#' Panel types include:
#' - Residual variance
#' - Interindividual covariance parameters
#' - Interindividual variance parameters
#' - Interoccasion variance parameters
#' - Covariate effect parameters
#' - Structural model parameters
#'4. Select columns for final tables.
#'
#' @param .df parameter estimates output from \code{\link[pmparams]{define_param_table}} with modifications ready for formatting
#' @param .select_cols columns to select for output. Default selects "type", "abb", "greek", "desc", "value", "ci", "shrinkage". To return all columns, specify "all" for .select_cols
#' @param .prse output pRSE. Default is FALSE
#' @param .digit number of significant digits. Default is three digits
#' @param .maxex maximum number of significant digits before moving to scientific notation. Default is NULL
#'
#' @examples
#'
#' #Using output from `define_param_table` (defineOut),
#' paramEst <- utils::read.csv(system.file("model/nonmem/param_est.csv", package = "pmparams"))
#' paramKey <-  system.file("model/nonmem/pk-parameter-key-new.yaml", package = "pmparams")
#' defineOut <- define_param_table(.estimates = paramEst, .key = paramKey, .ci = 95, .zscore = NULL)
#'
#' format_param_table(.df = defineOut)
#'
#' #To include all columns:
#'
#' format_param_table(.df = defineOut, .select_cols="all")
#' @export
format_param_table <- function(.df,
                             .select_cols = c("type", "abb", "greek", "desc", "value", "ci", "shrinkage"),
                             .prse = FALSE,
                             .digit = getOption("pmparams.dig"),
                             .maxex = getOption("pmparams.maxex")){

  .digit = ifelse(is.null(.digit), formals(pmtables::sig)$digits, .digit)

  .ci_level <- .df %>% dplyr::distinct(ci_level) %>% dplyr::pull(ci_level)
  .ci_final_nam <- paste0("ci_", .ci_level)
  .select_cols[.select_cols == "ci"] <- .ci_final_nam

  if (.prse == TRUE) {
    .df <- .df %>% getpRSE()
    .select_cols <- append(.select_cols, "pRSE")
  }

  .df_out <-
    .df %>%
    formatValues(.digit = .digit, .maxex = .maxex) %>%
    formatGreekNames() %>%
    getPanelName() %>%
    dplyr::arrange(as.numeric(nrow)) %>%
    dplyr::select(-nrow)

  .df_out[[paste0("ci_", .ci_level)]] <-  .df_out$ci

  if (any(tolower(.select_cols) == "all")) {
    return(.df_out %>% dplyr::select(-ci) %>% as.data.frame())
  } else {
    return(.df_out %>%
      dplyr::select(dplyr::all_of(.select_cols)) %>%
      as.data.frame())
  }




}
