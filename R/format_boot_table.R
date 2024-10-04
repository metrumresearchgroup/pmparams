#' Format bootstrap parameter table
#'
#' @description
#'
#' Format bootstrap parameter estimate values and output selected columns to be shown in
#' the bootstrap parameter table.
#'
#' @param .boot_df parameter estimates output from \code{\link[pmparams]{define_boot_table}} with modifications ready for formatting
#' @param .cleanup_cols clean up columns. Default TRUE and selects "abb", "desc", "boot_value", "boot_ci".
#' To return all columns, set .cleanup_cols = FALSE.
#' @param .digit set significant digits for output (optional). Default is three digits
#' @param .maxex set maxex for computation (optional). Default is NULL
#' @param .select_cols .select_cols columns to select for output. To return all columns, specify "all" for
#' .select_cols. Default is NULL.
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
                            .maxex = getOption("pmparams.maxex"),
                            .select_cols = NULL){

  .digit = ifelse(is.null(.digit), formals(pmtables::sig)$digits, .digit)

  if (length(.select_cols) > 0){
    message("`.select_cols` is included for backwards compatibility. Using this sets `.cleanup_cols` = F")
    .cleanup_cols = F
  }

  .df_out <- .boot_df %>%
    formatValuesBoot(.digit = .digit, .maxex = .maxex) %>%
    formatGreekNames() %>%
    mutate(diag = FALSE) %>% #TODO: Confirm with katherine
    getPanelName() %>%
    dplyr::arrange(as.numeric(nrow)) %>%
    dplyr::select(-nrow)

  if (length(.select_cols) > 0){
    if (!all(.select_cols %in% names(.df_out))){
      stop("Column selected does not exist in dataset")
    }
  }

  if (.cleanup_cols == F & length(.select_cols) == 0) {
    return(.df_out %>%
             as.data.frame()
    )
  } else if (.cleanup_cols == F & length(.select_cols) > 0){
    if (any(tolower(.select_cols) == "all")) {
      return(.df_out %>%
               as.data.frame()
      )
    } else {
      return(.df_out %>%
               dplyr::select(dplyr::all_of(.select_cols)) %>%
               as.data.frame()
      )
    }
  } else if (.cleanup_cols == T) {
    return(    return(.df_out %>%
                        dplyr::select(type, abb, desc, dplyr::starts_with("boot_value"), dplyr::starts_with("boot_perc"), dplyr::starts_with("perc")) %>%
                        as.data.frame())
    )
  }
}


