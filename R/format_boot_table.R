#' Format bootstrap parameter table
#'
#' @description
#'
#' Format bootstrap parameter estimate values and output selected columns to be shown in
#' the bootstrap parameter table.
#'
#' Left join this output bootstrap data.frame with the formatted output of non-bootstrap parameter estimates
#' TODO: Update format_param_table with .cleanup_cols
#' @param .boot_df parameter estimates output from \code{\link[pmparams]{define_boot_table}} with modifications ready for formatting
#' @param .cleanup_cols clean up columns. Default TRUE and selects "abb", "desc", "boot_value", "boot_ci". To return all columns, set .cleanup_cols = FALSE.
#' @param .digit set significant digits for output (optional). Default is three digits
#' @param .maxex set maxex for computation (optional). Default is NULL
#'
#' @examples
#'
#' #Using output from `define_boot_table` (defineBootOut),
#' boot_paramEst <- utils::read.csv(system.file("model/nonmem/boot/data/boot-106.csv",
#'                                  package = "pmparams"))
#' paramKey <-  system.file("model/nonmem/pk-parameter-key-new.yaml", package = "pmparams")
#'
#' defineBootOut <- define_boot_table(.boot_estimates = boot_paramEst,
#'                .key = paramKey)
#'
#' format_boot_table(.boot_df = defineBootOut)
#'
#' #To include all columns:
#'
#' format_boot_table(.boot_df = defineBootOut, .cleanup_cols = FALSE)
#' @export
format_boot_table <- function(.boot_df,
                            .cleanup_cols = TRUE,
                            .digit = getOption("pmparams.dig"),
                            .maxex = getOption("pmparams.maxex")){

  if (.cleanup_cols %in% c("T", "TRUE", T, TRUE)){
    .cleanup_cols = TRUE
  } else {
    .cleanup_cols = FALSE
  }

  .digit = ifelse(is.null(.digit), formals(pmtables::sig)$digits, .digit)

  .df_out <- .boot_df %>%
    formatValuesBoot(.digit = .digit, .maxex = .maxex) %>%
    dplyr::arrange(as.numeric(nrow)) %>%
    dplyr::select(-nrow)

  if (.cleanup_cols == TRUE) {
    return(.df_out %>%
             dplyr::select(abb, desc, dplyr::starts_with("boot_value"), dplyr::starts_with("boot_perc")) %>%
             as.data.frame())
  } else {
   return(.df_out %>%
     as.data.frame())
  }
}


