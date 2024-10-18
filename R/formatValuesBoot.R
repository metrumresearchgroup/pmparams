#' Format confidence interval percents for bootstrap run
#'
#' @note This function currently only rounds percent columns
#'
#' @inheritParams format_boot_table
#' @importFrom tidyselect all_of
#' @keywords internal
formatValuesBoot <- function(.boot_df,
                             .digit,
                             .maxex){
  # Gather percent columns
  perc_nam <- names(.boot_df)[grepl("perc", names(.boot_df))]

  # Round percent columns
  .boot_df <- .boot_df %>%
    dplyr::mutate(
      dplyr::across(all_of(perc_nam),  ~pmtables::sig(., .digit, .maxex))
    )
  return(.boot_df)
}
