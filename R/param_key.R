#' Parameter key requirements
#'
#' @description
#'
#' The parameter key file used in the mrgparamtab function must contain the following columns:
#'
#' __name__ = THETA, OMEGA, SIGMA
#'
#' __abb__ = abbreviation you want to appear in the parameter table (use latex coding)
#'
#' __desc__ = description you want to appear in the parameter table
#'
#' __panel__ = indicate which panel each parameter should appear under.
#'
#' Current options include:
#'
#'   - panel=="struct" ~ "Structural model parameters"
#'   - panel=="cov" ~ "Covariate effect parameters"
#'   - panel=="IIV" ~ "Interindividual covariance parameters" (off-diagonals << function takes care of this)
#'   - panel=="IIV" ~ "Interindividual variance parameters" (diagonals << function takes care of this)
#'   - panel=="IOV"  ~ "Interoccasion variance parameters"
#'   - panel=="RV" ~ "Residual variance"
#'
#'   Additional options can be added to `getPanelName` function in `functions-table.R`
#'
#' __trans__ = define how the parameter will be transformed. Current options include:
#'   - "none" -   untransformed parameters, e.g. THETAs or off-diagonals
#'   - "logTrans" -  THETAs estimated in the log domain
#'   - "logitTrans" - THETAs estimated using a logit transform
#'   - "lognormalOm"- for log-normal omegas e.g. CL = THETA(1) * exp(ETA(1)) - returns est.+CV%
#'   - "OmSD"       - for omegas where you want to return SD only - returns estimate & SD; use this for additive omegas e.g. CL = THETA(1) + ETA(1)
#'   - "logitOmSD"  - for omegas using logit transform - returns estimate & SD (calculated with logitnorm package); this option requires you provide the associated THETA separated with a "~"; e.g. "logitOmSD ~ THETA3"
#'   - "addErr"     - for additive error terms (coded using SIGMA in $ERROR) - returns est.+ SD
#'   - "propErr"    - for proportional error terms (coded using SIGMA in $ERROR) - returns est.+CV%
#'
#' @name param_key
#' @export

NULL
