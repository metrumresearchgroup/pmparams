#' Get value of standard error ???
#'
#' @export
getValueSE <- function(df){
  df %>%
    dplyr::mutate(value = estimate,
           se = stderr,
           corr_SD = case_when(OM & !diag |
                                 S & diag & addErr ~ sig(random_effect_sd),
                               TRUE ~ "-")
    )
}
