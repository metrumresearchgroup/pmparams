#' Format parameter table
#'
#' @description
#'
#' Format parameter estimate values and output selected columns to be shown in
#' the parameter table.
#'
#' There are four main steps of this function:
#'
#' 1. When necessary, back transform parameters and their CIs, round parameters
#' using `pmtables::sig`, and combine columns.
#'
#' 2. Format the THETA/OMEGA/SIGMA values to display as Greek letters in LaTeX,
#' with subscript numbers, and indicate the transformation applied to that
#' parameter, if applicable.
#'
#' 3. Determine which panel of the final table the parameter should be displayed
#' in. This is informed by the panel argument you defined in your parameter key.
#' Note that there are a finite number of options included by default (see
#' below), but you can include additional panels as needed.
#'
#'    Panel types include:
#'    - Residual variance
#'    - Interindividual covariance parameters
#'    - Interindividual variance parameters
#'    - Interoccasion variance parameters
#'    - Covariate effect parameters
#'    - Structural model parameters
#'
#' 4. Select columns for final tables.
#'
#' @param .df parameter estimates output from
#'   \code{\link[pmparams]{define_param_table}} with modifications ready for
#'   formatting.
#' @param .cleanup_cols logical (T/F). Defaults to `TRUE`, which selects the
#'   following columns:
#'   - `"type"`, `"abb"`, `"greek"`, `"desc"`, `"value"`, `"ci"`, `"shrinkage"`.
#'   - Set to `FALSE` to return all columns.
#' @param .prse logical (T/F). If `TRUE`, output `pRSE`. Default is `FALSE`.
#' @param .digit set significant digits for output (optional). Default is three
#'   digits.
#' @param .maxex set maxex for computation (optional). Default is `NULL`.
#' @param .select_cols Deprecated. Please use `.cleanup_cols` instead. Columns
#'   to select for output. To return all columns, specify `"all"`.
#'
#' @examples
#'
#' # Using output from `define_param_table` (defineOut),
#' paramEst <- utils::read.csv(system.file("model/nonmem/param_est.csv", package = "pmparams"))
#' paramKey <-  system.file("model/nonmem/pk-parameter-key-new.yaml", package = "pmparams")
#' defineOut <- define_param_table(.estimates = paramEst, .key = paramKey, .ci = 95, .zscore = NULL)
#'
#' format_param_table(.df = defineOut)
#'
#' # To include all columns:
#'
#' format_param_table(.df = defineOut, .cleanup_cols = FALSE)
#' @export
format_param_table <- function(
    .df,
    .cleanup_cols = TRUE,
    .prse = FALSE,
    .digit = getOption("pmparams.dig"),
    .maxex = getOption("pmparams.maxex"),
    .select_cols = NULL
){

  checkmate::assert_logical(.cleanup_cols)
  checkmate::assert_logical(.prse)

  .digit <- ifelse(is.null(.digit), formals(pmtables::sig)$digits, .digit)

  .ci_level <- unique(.df[["ci_level"]])
  .ci_final_nam <- paste0("ci_", .ci_level)

  if (isTRUE(.prse)) .df <- getpRSE(.df)

  # Create formatted table
  .df_out <-
    .df %>%
    formatValues(.digit = .digit, .maxex = .maxex) %>%
    formatGreekNames() %>%
    getPanelName() %>%
    dplyr::arrange(as.numeric(nrow))

  # Make ci column `ci_[ci_level]` (e.g., ci_95), instead of 'ci' and 'ci_level'
  .df_out[[paste0("ci_", .ci_level)]] <-  .df_out$ci
  .df_out <- .df_out %>% dplyr::select(-tidyselect::any_of(c("ci", "ci_level")))

  # Handle deprecated .select_cols arg, or define it below
  if (!is.null(.select_cols)){
    message("`.select_cols` is deprecated and included for backwards compatibility. Using this sets `.cleanup_cols = FALSE`")
    .cleanup_cols <- FALSE
    # Check for deprecated ci column specification
    if (any(tolower(.select_cols) == "ci")) {
      # TODO: test this
      message(glue::glue("`ci` is no longer a valid column name. pmparams will select {.ci_final_nam}"))
      .select_cols[.select_cols == "ci"] <- .ci_final_nam
      # .select_cols <- gsub("ci", {{.ci_final_nam}}, .select_cols, fixed = TRUE)
    }
    # Check for specified columns
    if (!(.select_cols %in% names(.df_out))){
      cols_missing <- setdiff(.select_cols, names(.df_out))
      cols_missing <- paste(cols_missing, collapse = ", ")
      stop(paste("The following specified columns were not found:", cols_missing))
    }
  } else {
    # Used when .cleanup_cols = TRUE
    .select_cols <- c("type", "abb", "greek", "desc", "value", "shrinkage", .ci_final_nam)
  }

  # Append pRES column if used
  if (isTRUE(.prse)) .select_cols <- append(.select_cols, "pRSE")

  select_everything <- isFALSE(.cleanup_cols) || any(tolower(.select_cols) == "all")
  if (isFALSE(select_everything)) {
    .df_out <- .df_out %>% dplyr::select(tidyselect::all_of(.select_cols))
  }

  return(.df_out)
}
