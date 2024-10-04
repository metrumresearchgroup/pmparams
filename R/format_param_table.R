#' Format parameter table
#'
#' @description
#'
#' Format parameter estimate values and output selected columns to be shown in
#' the parameter table.
#'
#' There are four main steps of this function:
#'1. When necessary, back transform parameters and their CIs, round parameters using pmtables::sig, and combine columns.
#'2. Format the THETA/OMEGA/SIGMA values to display as greek letters in Latex, with subscript numbers, and where necessary,
#'the transformation applied to that parameter.
#'3. Determine which panel of the final table the parameter should be displayed in. This is informed by the panel argument
#'you defined in your parameter key. Note that there are a finite number of options included by default (see below) but you
#'can include additional panels as needed.
#' Panel types include:
#' - Residual variance
#' - Interindividual covariance parameters
#' - Interindividual variance parameters
#' - Interoccasion variance parameters
#' - Covariate effect parameters
#' - Structural model parameters
#'4. Select columns for final tables.
#'
#' @param .df parameter estimates output from \code{\link[pmparams]{define_param_table}} with modifications
#' ready for formatting.
#' @param .cleanup_cols clean up columns. Default TRUE and selects "type", "abb", "greek", "desc", "value",
#' "ci", "shrinkage". To return all columns, specify .cleanup_cols = FALSE.
#' @param .prse output pRSE. Default is FALSE.
#' @param .digit set significant digits for output (optional). Default is three digits.
#' @param .maxex set maxex for computation (optional). Default is NULL.
#' @param .select_cols .select_cols columns to select for output. To return all columns, specify "all" for
#' .select_cols. Default is NULL.
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
#' format_param_table(.df = defineOut, .cleanup_cols = FALSE)
#' @export
format_param_table <- function(.df,
                             .cleanup_cols = T,
                             .prse = FALSE,
                             .digit = getOption("pmparams.dig"),
                             .maxex = getOption("pmparams.maxex"),
                             .select_cols = NULL){

  .digit = ifelse(is.null(.digit), formals(pmtables::sig)$digits, .digit)

  .ci_level <- .df %>% dplyr::distinct(ci_level) %>% dplyr::pull(ci_level)
  .ci_final_nam <- paste0("ci_", .ci_level)

  if (length(.select_cols) > 0){
    message("`.select_cols` is included for backwards compatibility. Using this sets `.cleanup_cols` = F")
    .select_cols[.select_cols == "ci"] <- .ci_final_nam
    .cleanup_cols = F
  } else {
    .cleanup_cols[.cleanup_cols == "ci"] <- .ci_final_nam
  }

  if (.prse == TRUE) {
    .df <- .df %>% getpRSE()
    if (length(.select_cols) > 0){
      .select_cols <- append(.select_cols, "pRSE")
    }
  }

  .df_out <-
    .df %>%
    formatValues(.digit = .digit, .maxex = .maxex) %>%
    formatGreekNames() %>%
    getPanelName() %>%
    dplyr::arrange(as.numeric(nrow)) %>%
    dplyr::select(-nrow)

  .df_out[[paste0("ci_", .ci_level)]] <-  .df_out$ci

  if (length(.select_cols) > 0 & any(tolower(.select_cols) == "ci")){
    message(glue::glue("`ci` is no longer a valid column name. pmparams will select ", {{.ci_final_nam}}))
    .select_cols <- gsub("ci", {{.ci_final_nam}}, .select_cols, fixed =T)
  }

  if (length(.select_cols) > 0){
    if (!(.select_cols %in% names(.df_out))){
      stop("Column selected does not exist in dataset")
    }
  }

  if (.cleanup_cols == F & length(.select_cols) == 0) {
          return(.df_out %>%
                   dplyr::select(-{{.ci_final_nam}}) %>%
                   as.data.frame()
          )
  } else if (.cleanup_cols == F & length(.select_cols) > 0){
          if (any(tolower(.select_cols) == "all")) {
            return(.df_out %>%
                     dplyr::select(-{{.ci_final_nam}}) %>%
                     as.data.frame()
            )
          } else {
            return(.df_out %>%
                     dplyr::select(dplyr::all_of(.select_cols)) %>%
                     as.data.frame()
            )
          }
  } else if (.cleanup_cols == T & .prse == F) {
    return(.df_out %>%
      dplyr::select("type", "abb", "greek", "desc", "value", "shrinkage", {{.ci_final_nam}}) %>%
      as.data.frame()
      )
  } else if (.cleanup_cols == T & .prse == T) {
    return(.df_out %>%
             dplyr::select("type", "abb", "greek", "desc", "value", "shrinkage", "pRSE", {{.ci_final_nam}}) %>%
             as.data.frame()
           )
  }


}
