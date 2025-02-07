#' Generate all footnote equation options
#' @description
#'
#' Generate data frame of generic footnote equations to append to parameter tables.
#'
#'
#' @inheritParams define_param_table
#'
#' @examples
#' model_dir <- system.file("model/nonmem", package = "pmparams")
#' paramKey <-  file.path(model_dir, "pk-parameter-key-new.yaml")
#'
#' # Read in parameter estimates (or provide `bbr` model)
#' param_est <- utils::read.csv(file.path(model_dir, "param_est_102.csv"))
#'
#' # Make and format parameter table
#' param_df <- param_est %>%
#'   define_param_table(paramKey, .ci = 95) %>%
#'   format_param_table()
#'
#' # Make random effects table and add relevant footnotes:
#' footnotes <- param_notes()
#'
#' table <- param_df %>%
#'   make_pmtable(.pmtype = "random") %>%
#'   pmtables::st_notes(footnotes$ci, footnotes$cv) %>%
#'   pmtables::st_notes_str() %>%
#'   pmtables::st_notes(footnotes$cvOmegaEq, footnotes$cvSigmaEq)
#'
#' @export
param_notes <- function(.ci = 95, .zscore = NULL){

  if(!is.null(.zscore)){
    lifecycle::deprecate_warn(
      when = "0.3.0",
      what = "define_param_table(.zscore)",
      details = "This argument is no longer used and will be ignored"
    )
  }

  alpha <- signif((1 - (.ci / 100)), 3)

  list(
    ci   = "CI: confidence intervals",
    corr = "Corr: correlation coefficient",
    cv   = "CV: coefficient of variation",
    rse = "RSE: relative standard error",
    se   = "SE: standard error",
    sd   = "SD: standard deviation",
    ciEq =  paste0("CI = estimate $\\pm$ $\\mathcal{Z}_{\\alpha/2}$ $\\cdot$ SE, $\\alpha = ", alpha, "$"),
    cvOmegaEq = "CV\\% of log-normal omegas = sqrt(exp(estimate) - 1) $\\cdot$ 100",
    cvSigmaEq = "CV\\% of sigma = sqrt(estimate) $\\cdot$ 100",
    logTrans = "Parameters estimated in the log-domain were back-transformed for clarity",
    logitTrans = "Parameters estimated in the logit-domain were back-transformed for clarity"
  )
}
