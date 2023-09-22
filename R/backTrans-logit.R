#' Back transform parameters estimated in the logit domain
#'
#' @description
#' Make sure any other calculations, such as CI (and pRSE) are
#' done before back-calculating these values.
#'
#' @keywords internal
backTrans_logit <- function(.df, .col_list = c("value", "lower", "upper")){
  for (i in 1:length(.col_list)){
    .col_name <- rlang::sym(.col_list[i])
    out <- .df %>%
      dplyr::mutate(
        !!.col_name := dplyr::case_when(LOGIT ~ exp(!!.col_name)/(1+exp(!!.col_name)), TRUE ~ !!.col_name)
      ) %>%
      dplyr::select(name, .col_name)

    .df2 <- .df %>%
      dplyr::select(-.col_name) %>%
      dplyr::left_join(out, by = "name")
  }
  .df2 <- .df2[names(.df)]
  .df2
}
