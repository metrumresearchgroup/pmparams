#' Define panel parameters
#'
#' @description
#' Define which parameters should appear under which panel name in the final table.
#'
#' @export
getPanelName = function(df){
  df %>%
    mutate(type = case_when(panel=="RV" ~ "Residual variance",
                            OM & !diag ~ "Interindividual covariance parameters",
                            OM & diag & panel=="IIV" ~ "Interindividual variance parameters",
                            # IOV not used here but included for convenience
                            OM & diag & panel=="IOV"  ~ "Interoccasion variance parameters",
                            panel=="cov" ~ "Covariate effect parameters",
                            panel=="struct" ~ "Structural model parameters"),
           # Make type a factor and use to sort, this ensures all parameters
           # of the same type are together - needed to make sure panels pull out
           # correct rows
           type_f = case_when(panel=="RV" ~ 6,
                              OM & !diag ~ 5,
                              OM & diag & panel=="IIV" ~ 3,
                              # IOV not used here but included for convenience
                              OM & diag & panel=="IOV"  ~ 4,
                              panel=="cov" ~ 2,
                              panel=="struct" ~ 1)
    ) %>%
    arrange(type_f)
}
