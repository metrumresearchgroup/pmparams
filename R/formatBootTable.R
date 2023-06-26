#' Format bootstrap parameter table
#'
#' @description
#'
#' Format bootstrap parameter estimate values and output selected columns to be shown in
#' the bootstrap parameter table.
#'
#' Left join this output bootstrap data.frame with the formatted output of non-bootstrap parameter estimates
#'
#' @param .boot_df parameter estimates output from `defineBootTable` with modifications ready for formatting
#' @param .select_cols columns to select for output. Default selects "abb", "desc", "boot_value", "boot_ci". To return all columns, specify "all" for .select_cols.
#' @param .digit set significant digits for output (optional). Default is three digits
#' @param .maxex set maxex for computation (optional). Default is NULL
#'
#' @examples
#'
#' #Using output from `defineBootTable` (defineBootOut),
#' boot_paramEst <- utils::read.csv(system.file("model/nonmem/boot/data/boot-106.csv",
#'                                  package = "mrgparamtab"))
#' nonboot_paramEst <- utils::read.csv(system.file("model/nonmem/nonboot_param_est.csv",
#'                                     package = "mrgparamtab"))
#' paramKey <-  system.file("model/nonmem/pk-parameter-key-new.yaml", package = "mrgparamtab")
#'
#' defineBootOut <- defineBootTable(.boot_estimates = boot_paramEst,
#'                .nonboot_estimates = nonboot_paramEst,
#'                .key = paramKey)
#'
#' formatBootTable(.boot_df = defineBootOut)
#'
#' #To include all columns:
#'
#' formatBootTable(.boot_df = defineBootOut, .select_cols="all")
#' @export
formatBootTable <- function(.boot_df,
                            .select_cols = c("abb", "desc", "boot_value", "boot_ci"),
                            .digit = getOption("mrgparamtab.dig"),
                            .maxex = getOption("mrgparamtab.maxex")){

  .digit = ifelse(is.null(.digit), formals(pmtables::sig)$digits, .digit)

  .df_out <- .boot_df %>%
    formatValuesBoot()

  if (any(tolower(.select_cols) == "all")) {
    return(.df_out %>% as.data.frame())
  } else {
    return(.df_out %>%
             dplyr::select(.select_cols) %>%
             as.data.frame())
  }

  .df_out


}


