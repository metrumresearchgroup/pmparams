#' Determine a value (or estimate) for each model parameter and it’s associated metric
#'
#'@description
#' value should have estimate [something]:
#'
#'   theta = estimate only                           # use estimate column
#'
#'   omega diagonals = variance [%CV]                # estimate [CV from estimate, stderr]
#'
#'   omega off-diagonals = covariance [corr coeff]   # estimate [random_effect_sd]
#'
#'   sigma diagonal proportional = variance [%CV]    # estimate [CV from estimate, stderr]
#'
#'   sigma diagonal additive = variance [SD]         # estimate [random_effect_sd]
#'
#' @param .df data.frame with parameter estimates
#' @param .digit set the number of significant digits
#' @param .maxex set the number of maxex
#'
#' @keywords internal
getValueSE <- function(.df, .digit, .maxex){

  .df %>%
    dplyr::mutate(
      value = estimate,
      se = stderr,
      corr_SD = dplyr::case_when(
        OM & !diag | S & diag & addErr ~ pmtables::sig(random_effect_sd, .digit, .maxex),
        TRUE ~ "-")
    )
}

