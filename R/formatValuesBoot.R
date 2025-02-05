#' Format confidence interval percents for bootstrap run
#'
#' @note This function currently only rounds percent columns
#'
#' @inheritParams format_boot_table
#' @importFrom tidyselect all_of
#' @keywords internal
formatValuesBoot <- function(
    .boot_df,
    .digit,
    .maxex
){
  ci_name <- paste0("boot_ci_", unique(.boot_df[["ci_level"]]))

  .boot_df %>%
    dplyr::mutate(
      boot_value = pmtables::sig(.data$value, .digit, .maxex),
      fixed = (.data$lower == .data$upper),
      !!rlang::sym(ci_name) := dplyr::if_else(
        .data$fixed,
        "FIXED",
        paste0(
          pmtables::sig(.data$lower, .digit, .maxex), ', ',
          pmtables::sig(.data$upper, .digit, .maxex)
        )
      )
    ) %>%
    dplyr::select(-"fixed")
}
