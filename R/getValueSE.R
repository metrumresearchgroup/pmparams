#' Get value or estimate for each model parameter and its associated metric
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
#' @export
getValueSE <- function(.df,
                       .digit = getOption("mrgparamtab.dig"),
                       .maxex = getOption("mrgparamtab.maxex")){

  .digit = ifelse(is.null(.digit), formals(pmtables::sig)$digits, .digit)

  .df %>%
    dplyr::mutate(
      value = estimate,
      se = stderr,
      corr_SD = dplyr::case_when(
        OM & !diag | S & diag & addErr ~ pmtables::sig(random_effect_sd, .digit, .maxex),
        TRUE ~ "-")
    )
}

