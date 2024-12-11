#' Format bootstrap parameter table
#'
#' @description
#'
#' Format bootstrap parameter estimate values and output selected columns to be shown in
#' the bootstrap parameter table.
#'
#' Left join this output bootstrap data.frame with the formatted output of non-bootstrap parameter estimates
#'
#' @param .boot_df parameter estimates output from \code{\link[pmparams]{define_boot_table}} with modifications ready for formatting
#' @param .select_cols columns to select for output. Default selects "abb", "desc", "boot_value", "boot_ci". To return all columns, specify "all" for .select_cols.
#' @param .digit set significant digits for output (optional). Default is three digits
#' @param .maxex set maxex for computation (optional). Default is NULL
#'
#' @examples
#'
#' #Using output from `define_boot_table` (defineBootOut),
#' boot_paramEst <- utils::read.csv(system.file("model/nonmem/boot/data/boot-106.csv",
#'                                  package = "pmparams"))
#' nonboot_paramEst <- utils::read.csv(system.file("model/nonmem/param_est_106.csv",
#'                                     package = "pmparams"))
#' paramKey <-  system.file("model/nonmem/pk-parameter-key-new.yaml", package = "pmparams")
#'
#' defineBootOut <- define_boot_table(.boot_estimates = boot_paramEst,
#'                .nonboot_estimates = nonboot_paramEst,
#'                .key = paramKey)
#'
#' format_boot_table(.boot_df = defineBootOut)
#'
#' #To include all columns:
#'
#' format_boot_table(.boot_df = defineBootOut, .select_cols="all")
#' @export
format_boot_table <- function(.boot_df,
                            .select_cols = c("abb", "desc", "boot_value", "boot_ci_95"),
                            .digit = getOption("pmparams.dig"),
                            .maxex = getOption("pmparams.maxex")){

  .digit = ifelse(is.null(.digit), formals(pmtables::sig)$digits, .digit)

  .df_out <- .boot_df %>%
    formatValuesBoot(.digit = .digit, .maxex = .maxex) %>%
    dplyr::arrange(as.numeric(nrow)) %>%
    dplyr::select(-nrow)

  if (any(tolower(.select_cols) == "all")) {
    return(.df_out %>% as.data.frame())
  } else {
    return(.df_out %>%
             dplyr::select(dplyr::all_of(.select_cols)) %>%
             as.data.frame())
  }

  .df_out


}


