#' Get value of standard error
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
#' @param .estimate column name of estimate
#' @param .stderr column name of standard error of estimate (STDERR)
#' @param .om column name of OMEGA
#' @param .digit set significant digits for output (optional)
#' @param .maxex set maxex for computation (optional)
#'
#' @export
getValueSE <- function(.df, .estimate = "estimate", .stderr = "stderr",
                       .om = "OM", .diag = "diag", .s = "S", .addErr = "addErr",
                       .random_effect_sd = "random_effect_sd",
                       .digit = getOption("mrgparamtab.dig"),
                       .maxex = getOption("mrgparamtab.maxex")){

  .digit = ifelse(is.null(.digit), formals(pmtables::sig)$digits, .digit)

  df %>%
    dplyr::mutate(value = .estimate,
                  se = .stderr,
                  corr_SD = case_when(.om & !.diag |
                            .s & .diag & .addErr ~ pmtables::sig(.random_effect_sd, .digit, .maxex),
                            TRUE ~ "-")
    )
}

