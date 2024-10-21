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
    if (any(tolower(.select_cols) == "all")) .select_cols <- names(.df_out)
    # Check for specified columns
    if (!all(.select_cols %in% names(.df_out))){
      cols_missing <- setdiff(.select_cols, names(.df_out))
      cols_missing <- paste(cols_missing, collapse = ", ")
      stop(paste("The following specified columns were not found:", cols_missing))
    }
  } else {
    # Used when .cleanup_cols = TRUE
    if (isTRUE(.cleanup_cols)) {
      ci_cols <- names(.df_out)[grepl("^perc_", names(.df_out))]
      .select_cols <- c("abb", "desc", ci_cols)
    } else {
      .select_cols <- names(.df_out)
    }
  }

  .df_out <- .df_out %>% dplyr::select(all_of(.select_cols))

  # Format and group bootstrap `perc_[x]` columns
  .df_final <- format_boot_cols(.df_out)

  return(.df_final)
}

#' Format and group bootstrap `perc_[x]` columns
#'
#' Format and group bootstrap `perc_[x]` columns. Columns that correspond to a
#' confidence interval will instead be grouped into a new confidence interval
#' column displaying the range. E.g., `perc_2.5` + `perc_97.5` --> `95% CI`
#' @inheritParams make_boot_pmtable
#' @keywords internal
format_boot_cols <- function(.df){
  # Extract bootstrap columns and calculate boot values
  boot_cols_keep <- names(.df)[grepl("perc_", names(.df))]
  boot_values <- as.numeric(gsub("perc_", "", boot_cols_keep))

  # This determines which values are paired to another (e.g., 5% and 95%)
  #  - Remove the median (50%) from boot_values before finding CI pairs
  filtered_boot_values <- boot_values[boot_values != 50]
  ci_pairs <- which(filtered_boot_values + rev(filtered_boot_values) == 100)

  new_columns <- c() # Keep track of new columns

  # Create confidence intervals by pairing lower and upper percentiles
  if (length(ci_pairs) > 0 && length(ci_pairs) %% 2 == 0) {
    upper_indices <- length(boot_values) - ci_pairs + 1
    paired_indices <- as.vector(rbind(ci_pairs, upper_indices))[seq_along(ci_pairs)]
    paired_cols <- boot_cols_keep[paired_indices]

    lower_vals <- boot_values[paired_indices[seq(1, length(paired_indices), by = 2)]]
    upper_vals <- boot_values[paired_indices[seq(2, length(paired_indices), by = 2)]]
    ci_names <- paste0("boot_ci_", upper_vals - lower_vals)

    # Add new CI columns to the dataframe
    new_ci_cols <- purrr::map2_dfc(
      split(paired_cols, rep(1:(length(paired_cols)/2), each = 2)),
      ci_names,
      ~ purrr::set_names(
        list(paste(.df[[.x[1]]], .df[[.x[2]]], sep = ", ")),
        .y
      )
    )

    # Substitute boot_ci_50 for boot_ci_iqr if present
    # - That column can only originate from iqr (or .percentiles = c(0.25, 0.75))
    # because they must be paired (add up to 100) _and_ the difference must be 50
    if ("boot_ci_50" %in% names(new_ci_cols)){
      new_ci_cols <- new_ci_cols %>% dplyr::rename("boot_ci_iqr" = "boot_ci_50")
    }

    new_columns <- c(new_columns, names(new_ci_cols))
    .df <- dplyr::bind_cols(.df, new_ci_cols)
  } else {
    paired_indices <- numeric(0)
  }

  # Find & handle remaining columns that are not paired to a CI (e.g., median)
  remaining_indices <- setdiff(seq_along(boot_values), paired_indices)
  remaining_cols <- boot_cols_keep[remaining_indices]
  remaining_values <- boot_values[remaining_indices]

  renamed_cols <- .df %>%
    dplyr::rename_with(
      .fn = function(remain_col){
        dplyr::case_when(
          remain_col %in% remaining_cols[remaining_values == 50] ~ "boot_median",
          TRUE ~ paste0("boot_", remain_col)
        )},
      .cols = all_of(remaining_cols)
    )

  new_columns <- c(new_columns, setdiff(names(renamed_cols), names(.df)))

  # Remove any old columns that still exist
  .df_final <- renamed_cols %>% dplyr::select(-any_of(boot_cols_keep))
  attr(.df_final, "new_columns") <- new_columns

  return(.df_final)
}
