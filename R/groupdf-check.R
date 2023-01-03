#' Confirm data frames are not grouped
#'
#' @description
#'
#' Returns a warning if an input dataframe is grouped when given to a function not
#' anticipating a grouped column.
#'
#' @keywords internal
grouped_warn_ungroup <- function(.df, .func) {
  if (inherits(.df, "grouped_df")) {
    warning(glue::glue("Grouping found in data.frame. `{.func}()` evaluates a single row at a time. Automatically ungrouping data.frame."), call. = F)
  }
  return(dplyr::ungroup(.df))
}
