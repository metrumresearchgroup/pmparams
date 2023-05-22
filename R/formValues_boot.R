#' Define how values are to be displayed
#'
#' @description
#' Format values for bootstrap run
#'
#' @keywords internal

formatValues_boot <- function(df){
  df %>%
    mutate(boot_ci = paste0(sig(lower), ', ', sig(upper)),
           boot_ci = if_else(fixed, "FIXED", boot_ci),
           boot_value = sig(value))
}
