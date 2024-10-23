#' Generate all footnote equation options
#' @description
#'
#' Generate data frame of generic footnote equations to append to parameter tables.
#'
#'
#' @param .ci specify 90 or 95 percent confidence interval (default 95%)
#' @param .zscore z-score for the specified confidence interval. Only needed for
#'  confidence intervals that are NOT 90 or 95 percent
#'
#' @examples
#' library(dplyr)
#' model_dir <- system.file("model/nonmem", package = "pmparams")
#' paramKey <-  file.path(model_dir, "pk-parameter-key-new.yaml")
#'
#' # Read in parameter estimates (or provide `bbr` model)
#' param_est <- utils::read.csv(file.path(model_dir, "param_est.csv"))
#'
#' # Make and format parameter table
#' defineOut <- define_param_table(
#'  .estimates = param_est,
#'  .key = paramKey,
#'  .ci = 95
#' ) %>% format_param_table()
#'
#' # Make random effects table and add relevant footnotes:
#' footnotes <- param_notes()
#'
#' table <- make_pmtable(.df = data, .pmtype = "random") %>%
#'   pmtables::st_notes(footnotes$ci, footnotes$cv) %>%
#'   pmtables::st_notes_str() %>%
#'   pmtables::st_notes(footnotes$cvOmegaEq, footnotes$cvSigmaEq)
#'
#' @export
param_notes <- function(.ci = 95, .zscore = NULL){

  .validated_zscore <-
    if (is.null(.zscore)){
      if (.ci == 95){
        1.96
      } else if(.ci == 90){
        1.64
      } else {
        stop("Z-score (.zscore) must be supplied when CI is not 90 or 95")
      }
    } else {
      if (.ci == 95 & .zscore != 1.96){
        zscore_ci_msg(.ci = 95, .zscore= 1.96)
        1.96
      } else if(.ci == 90 & .zscore != 1.64){
        zscore_ci_msg(.ci = 90, .zscore= 1.64)
        1.64
      } else {
        .zscore
      }
    }


  list(
    ci   = "CI: confidence intervals",
    corr = "Corr: correlation coefficient",
    cv   = "CV: coefficient of variation",
    iqr = "IQR: interquartile range",
    rse = "RSE: relative standard error",
    se   = "SE: standard error",
    sd   = "SD: standard deviation",
    ciEq =  paste0("CI = estimate $\\pm$ ", .validated_zscore, " $\\cdot$ SE"),
    cvOmegaEq = "CV\\% of log-normal omegas = sqrt(exp(estimate) - 1) $\\cdot$ 100",
    cvSigmaEq = "CV\\% of sigma = sqrt(estimate) $\\cdot$ 100",
    logTrans = "Parameters estimated in the log-domain were back-transformed for clarity",
    logitTrans = "Parameters estimated in the logit-domain were back-transformed for clarity"
  )
}
