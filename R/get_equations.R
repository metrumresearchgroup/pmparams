#' Generate all footnote equation options
#' @description
#'
#' Generate data frame of generic footnote equations to append to parameter tables.
#'
#'
#' @param .ci confidence interval. Default is 95
#'
#' @examples
#' #Using output from `format_param_table` (defineOut),
#' paramEst <- utils::read.csv(system.file("model/nonmem/param_est.csv", package = "pmparams"))
#' paramKey <-  system.file("model/nonmem/pk-parameter-key-new.yaml", package = "pmparams")
#' defineOut <- define_param_table(.estimates = paramEst, .key = paramKey, .ci = 95, .zscore = NULL)
#' data <- format_param_table(.df = defineOut)
#'
#' #To make random effects table and add relevant footnotes:
#' footnotes = get_equations()
#' make_pmtable(.df = data, .pmtype = "random") %>%
#' pmtables::st_notes(paste0("Abbreviations: ", footnotes$ci),footnotes$cv, collapse= "; ", to_string = TRUE) %>%
#' pmtables::st_notes(footnotes$cvOmegaEq, footnotes$cvSigmaEq)
#'
#'
#' @export
get_equations <- function(.ci = 95, .zscore = NULL){

  if (.ci == 95){
    .zscore = 1.64
  } else if (.ci == 90){
    .zscore = 1.96
  } else if (!(.ci %in% c(90, 95)) & !is.na(.zscore)){
    .zscore = .zscore
  } else {
    stop("Z-score (.zscore) must be supplied when CI is not 90 or 95")
  }

  x <- list(
    ci   = "CI: confidence intervals",
    corr = "Corr: Correlation coefficient",
    cv   = "CV: coefficient of variation",
    se   = "SE: standard error",
    sd   = "SD: standard deviation",
    ciEq =  paste0("Confidence intervals = estimate $\\pm$ ", .zscore, " $\\cdot$ SE"),
    cvOmegaEq = "CV\\% of log-normal omegas = sqrt(exp(estimate) - 1) $\\cdot$ 100",
    cvSigmaEq = "CV\\% of sigma = sqrt(estimate) $\\cdot$ 100",
    logTrans = "Parameters estimated in the log-domain were back-transformed for clarity.",
    logitTrans = "Parameters estimated in the logit-domain were back-transformed for clarity."
  )

  return(x)

}
