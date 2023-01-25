#' Define panel parameters
#'
#' @description
#' Define which parameters should appear under which panel name in the final table.
#'
#' Panel types include:
#' - Residual variance
#' - Interindividual covariance parameters
#' - Interindividual variance parameters
#' - Interoccasion variance parameters
#' - Covariate effect parameters
#' - Structural model parameters
#'
#' @keywords internal
getPanelName = function(.df){
  .df %>%
    dplyr::mutate(
      type = dplyr::case_when(panel=="RV" ~ "Residual variance",
                              OM & !diag ~ "Interindividual covariance parameters",
                              OM & diag & panel=="IIV" ~ "Interindividual variance parameters",
                              OM & diag & panel=="IOV"  ~ "Interoccasion variance parameters",
                              panel=="cov" ~ "Covariate effect parameters",
                              panel=="struct" ~ "Structural model parameters"),

      type_f = dplyr::case_when(panel=="RV" ~ 6,
                         OM & !diag ~ 5,
                         OM & diag & panel=="IIV" ~ 3,
                         OM & diag & panel=="IOV"  ~ 4,
                         panel=="cov" ~ 2,
                         panel=="struct" ~ 1)
    ) %>%
    dplyr::arrange(.df$type_f)
}
