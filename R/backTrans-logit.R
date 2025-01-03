#' Back transform parameters estimated in the logit domain
#'
#' @description
#' Make sure any other calculations, such as CI (and pRSE) are
#' done before back-calculating these values.
#'
#' @keywords internal
backTrans_logit <- function(.df){

  if (any(grepl("perc", names(.df)))){
    .df %>%
      dplyr::mutate_at(
        dplyr::vars(dplyr::contains("perc")),
        ~ dplyr::case_when(
          LOGIT ~ stats::plogis(.),
          TRUE ~ .
        )
      )
  } else {
    .df %>%
      dplyr::mutate_at(
        dplyr::vars("value", "lower", "upper"),
        ~ dplyr::case_when(
          LOGIT ~ stats::plogis(.),
          TRUE ~ .
        )
      )
  }

}
