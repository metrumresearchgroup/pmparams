#' Calculate CV%
#'
#' @export
getpCV <- function(.df){
  .df %>%
    dplyr::mutate(cv = dplyr::case_when(
      diag & OM & lognormO ~ sig(getCV_lognormO(value)),
      diag & S & propErr ~ sig(getCV_propS(value)),
      TRUE ~ "-"))
}
