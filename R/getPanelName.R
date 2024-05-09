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
                              panel=="IIV" ~ "Interindividual variance parameters",
                              panel=="IOV"  ~ "Interoccasion variance parameters",
                              panel=="cov" ~ "Covariate effect parameters",
                              panel=="struct" ~ "Structural model parameters"),

      type_f = dplyr::case_when(panel=="RV" ~ 6,
                         OM & !diag ~ 5,
                         panel=="IIV" ~ 3,
                         panel=="IOV"  ~ 4,
                         panel=="cov" ~ 2,
                         panel=="struct" ~ 1)
    ) %>%
    dplyr::arrange(type_f)
}
