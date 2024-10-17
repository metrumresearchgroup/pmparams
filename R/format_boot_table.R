#' Format bootstrap parameter table
#'
#' Format bootstrap parameter estimate values and output selected columns to be shown in
#' the bootstrap parameter table.
#'
#' @param .boot_df parameter estimates output from [define_boot_table()] with
#'   modifications ready for formatting
#' @param .cleanup_cols logical (T/F). Defaults to `TRUE`, which selects the
#'   following columns:
#'   - `"abb"`, `"desc"`, `"boot_value"`, `"boot_ci`".
#'   - Set to `FALSE` to return all columns.
#' @param .digit set significant digits for output (optional). Default is three digits
#' @param .maxex set maxex for computation (optional). Default is `NULL`
#' @param .select_cols Deprecated. Please use `.cleanup_cols` instead. Columns
#'   to select for output. To return all columns, specify `"all"`.
#'
#' @examples
#'
#' model_dir <- system.file("model/nonmem", package = "pmparams")
#' paramKey <-  file.path(model_dir, "pk-parameter-key-new.yaml")
#'
#' # Using a file path:
#' boot_path <- file.path(model_dir, "boot/data/boot-106.csv")
#' boot_df <- define_boot_table(
#'  .boot_estimates = boot_path,
#'  .key = paramKey
#' )
#'
#' format_boot_table(boot_df)
#'
#' # To include all columns:
#' format_boot_table(boot_df, .cleanup_cols = FALSE)
#'
#' # Using a `bbr` bootstrap model object:
#' \dontrun{
#' boot_run <- bbr::read_model(file.path(model_dir, "106-boot"))
#' boot_df <- define_boot_table(
#'  .boot_estimates = bbr::bootstrap_estimates(boot_run),
#'  .key = paramKey
#' )
#'
#' format_boot_table(boot_df)
#' }
#'
#' @export
format_boot_table <- function(
    .boot_df,
    .cleanup_cols = TRUE,
    .digit = getOption("pmparams.dig"),
    .maxex = getOption("pmparams.maxex"),
    .select_cols = NULL
){
  checkmate::assert_logical(.cleanup_cols)
  .digit <- ifelse(is.null(.digit), formals(pmtables::sig)$digits, .digit)

  # Create formatted table
  .df_out <- .boot_df %>%
    formatValuesBoot(.digit = .digit, .maxex = .maxex) %>%
    dplyr::arrange(as.numeric(nrow))

  # Handle deprecated .select_cols arg, or define it below
  if (!is.null(.select_cols)){
    message("`.select_cols` is deprecated and included for backwards compatibility. Using this sets `.cleanup_cols = FALSE`")
    .cleanup_cols <- FALSE
    # Check for specified columns
    if (!(.select_cols %in% names(.df_out))){
      cols_missing <- setdiff(.select_cols, names(.df_out))
      cols_missing <- paste(cols_missing, collapse = ", ")
      stop(paste("The following specified columns were not found:", cols_missing))
    }
  } else {
    # Used when .cleanup_cols = TRUE
    ci_cols <- names(.df_out)[grepl("^perc_", names(.df_out))]
    .select_cols <- c("abb", "desc", ci_cols)
  }

  select_everything <- isFALSE(.cleanup_cols) || any(tolower(.select_cols) == "all")
  if (isFALSE(select_everything)) {
    .df_out <- .df_out %>% dplyr::select(tidyselect::all_of(.select_cols))
  }

  return(.df_out)
}


