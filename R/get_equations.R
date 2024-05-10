#' Generate all footnote equation options
#' @description
#'
#' Generate data frame of generic footnote equations to append to parameter tables
#'
#'
#' @param .ci confidence interval. Default is 95
#'
#' @examples
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
    ciEq =  paste0("Confidence intervals = estimate $\\pm$ ", .zscore, " $\\cdot$ SE"),
    cvOmegaEq = "CV\\% of log-normal omegas = sqrt(exp(estimate) - 1) $\\cdot$ 100",
    cvSigmaEq = "CV\\% of sigma = sqrt(estimate) $\\cdot$ 100",
    log = "Parameters estimated in the log-domain were back-transformed for clarity.",
    logit = "Parameters estimated in the logit-domain were back-transformed for clarity."
  )

  return(x)

}
