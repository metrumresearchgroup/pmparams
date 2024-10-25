#' Format parameter table
#'
#' Format parameter estimate values and output selected columns to be shown in
#' the parameter table.
#'
#' @details
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
#' @param .df parameter estimates output from [define_param_table()] with
#'   modifications ready for formatting.
#' @param .cleanup_cols logical (T/F). Defaults to `TRUE`, which selects the
#'   following columns:
#'   - `"type"`, `"abb"`, `"greek"`, `"desc"`, `"value"`, `"ci"`, `"shrinkage"`.
#'   - Set to `FALSE` to return all columns.
#' @param .prse logical (T/F). If `TRUE`, output `pRSE`. Default is `FALSE`.
#' @param .digit number of significant digits. Default is three digits
#' @param .maxex maximum number of significant digits before moving to scientific
#'   notation. Default is NULL
#' @param .select_cols Deprecated. Please use `.cleanup_cols` instead.
#'
#' @examples
#'
#' model_dir <- system.file("model/nonmem", package = "pmparams")
#' paramKey <-  file.path(model_dir, "pk-parameter-key-new.yaml")
#'
#' # Using a file path:
#' param_ests <- readr::read_csv(file.path(model_dir, "param_est.csv"))
#' param_df <- define_param_table(
#'  .estimates = param_ests,
#'  .key = paramKey,
#'  .ci = 95,
#' )
#'
#' format_param_table(param_df)
#'
#' # To include all columns:
#' format_param_table(param_df, .cleanup_cols = FALSE)
#'
#' # Using a `bbr` model
#' \dontrun{
#' mod <- bbr::read_model(file.path(model_dir, "106"))
#' param_df <- define_param_table(
#'  .estimates = mod,
#'  .key = paramKey,
#'  .ci = 95,
#' ) %>% format_param_table()
#' }
#'
#' @importFrom tidyselect any_of all_of
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
  .df_out <- .df_out %>% dplyr::select(-any_of(c("ci", "ci_level")))

  # Handle deprecated .select_cols arg, or define it below
  if (!is.null(.select_cols)){
    warning("`.select_cols` is deprecated and included for backwards compatibility. Using this sets `.cleanup_cols = FALSE`")
    .cleanup_cols <- FALSE
    # Check for deprecated ci column specification
    if (any(tolower(.select_cols) == "ci")) {
      message(glue::glue("`ci` is no longer a valid column name. pmparams will select {.ci_final_nam}"))
      .select_cols[.select_cols == "ci"] <- .ci_final_nam
    }
    if (any(tolower(.select_cols) == "all")) .select_cols <- names(.df_out)
    # Check for specified columns
    if (!all(.select_cols %in% names(.df_out))){
      cols_missing <- setdiff(.select_cols, names(.df_out))
      cols_missing <- paste(cols_missing, collapse = ", ")
      stop(paste("The following specified columns were not found:", cols_missing))
    }
  } else {
    if (isTRUE(.cleanup_cols)) {
      .select_cols <- c("type", "abb", "greek", "desc", "value", "shrinkage", .ci_final_nam)
    } else {
      .select_cols <- names(.df_out)
    }
  }

  # Append pRES column if used
  if (isTRUE(.prse)) .select_cols <- append(.select_cols, "pRSE")

  .df_out <- .df_out %>% dplyr::select(all_of(.select_cols))

  return(.df_out)
}
