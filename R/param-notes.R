#' Generate all footnote equation options
#'
#' Generate list of generic footnote equations to append to parameter tables.
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
#' footnotes <- param_notes(.ci = 95)
#'
#' table <- param_df %>%
#'   make_pmtable(.pmtype = "random") %>%
#'   pmtables::st_notes(footnotes$ci, footnotes$cv) %>%
#'   pmtables::st_notes_str() %>%
#'   pmtables::st_notes(footnotes$ciEq) %>%
#'   pmtables::st_notes(footnotes$cvOmegaEq, footnotes$cvSigmaEq)
#'
#' @seealso [boot_notes()]
#' @export
param_notes <- function(.ci = 95, .zscore = NULL){

  if(!checkmate::test_integerish(.ci, lower = 1, upper = 99, len = 1)){
    rlang::abort("`.ci` must be between 1 and 99")
  }

  if(!is.null(.zscore)){
    lifecycle::deprecate_warn(
      when = "0.3.0",
      what = "define_param_table(.zscore)",
      details = "This argument is no longer used and will be ignored"
    )
  }

  alpha <- signif((1 - (.ci / 100)), 3)

  list(
    ci   = "CI: confidence interval",
    corr = "Corr: correlation coefficient",
    cv   = "CV: coefficient of variation",
    rse = "RSE: relative standard error",
    se   = "SE: standard error",
    sd   = "SD: standard deviation",
    ciEq =  paste0("CI = estimate $\\pm$ $z_{\\alpha/2}$ $\\cdot$ SE, $\\alpha = ", alpha, "$"),
    cvOmegaEq = "CV\\% of log-normal omega = sqrt(exp(estimate) - 1) $\\cdot$ 100",
    cvSigmaEq = "CV\\% of sigma = sqrt(estimate) $\\cdot$ 100",
    logTrans = "Parameters estimated in the log domain were back-transformed for clarity",
    logitTrans = "Parameters estimated in the logit domain were back-transformed for clarity"
  )
}


#' Generate footnotes for a bootstrap table
#'
#' Generate list of footnotes to append to bootstrap parameter tables.
#'
#' @param .ci Confidence interval. A value from 1 to 99 denoting the percent
#' confidence interval.
#' @param .n_run Number of runs in the bootstrap
#'
#' @examples
#' \dontrun{
#' model_dir <- system.file("model/nonmem", package = "pmparams")
#' paramKey <-  file.path(model_dir, "pk-parameter-key-new.yaml")
#' mod <- bbr::read_model(file.path(model_dir, "106"))
#' boot_run <- bbr::read_model(file.path(model_dir, "106-boot"))
#'
#' # Parameter Estimates
#' param_df <- mod %>%
#'   define_param_table(paramKey, .ci = 95) %>%
#'   format_param_table()
#'
#' # Bootstrap Estimates
#' boot_est <- bbr::bootstrap_estimates(boot_run)
#' boot_df <- boot_est %>%
#'   define_boot_table(paramKey, .ci = 95) %>%
#'   format_boot_table()
#'
#' # Combine
#' combine_df <- dplyr::left_join(param_df, boot_df)
#'
#' # Make random effects table and add relevant footnotes:
#' footnotes <- param_notes()
#' bootnotes <- boot_notes(.ci = 95, .n_run = nrow(boot_est))
#'
#' combine_df %>%
#'   make_boot_pmtable(.pmtype = "random") %>%
#'   pmtables::st_notes(footnotes$ci, footnotes$cv) %>%
#'   pmtables::st_notes_str() %>%
#'   pmtables::st_notes(bootnotes$boot_ci)  %>%
#'   pmtables::st_notes(footnotes$cvOmegaEq, footnotes$cvSigmaEq)  %>%
#'   pmtables::stable() %>%
#'   # preview in Rstudio viewer (requires `magick` and `pdftools`)
#'   pmtables::st_as_image(border = "0.8cm 0.7cm")
#' }
#' @seealso [param_notes()]
#' @export
boot_notes <- function(.ci = NULL, .n_run = NULL){
  if(is.null(.ci)){
    lower <- "x"; upper <- "y"
    rlang::warn(".ci was not provided, so a a placeholder will be used.")
  }else{
    if(!checkmate::test_integerish(.ci, lower = 1, upper = 99, len = 1)){
      rlang::abort("`.ci` must be between 1 and 99")
    }
    lower <- (100 - .ci) / 2
    upper <- 100 - lower
  }

  if(is.null(.n_run)){
    .n_run <- "xx"
    rlang::warn(".n_run was not provided, so a a placeholder will be used.")
  }else{
    if(!checkmate::test_integerish(.n_run, lower = 1, len = 1)){
      rlang::abort("`.n_run` must be an integer greater than 1.")
    }
  }


  list(
    boot_ci = paste0(
      "The CI was determined from the ", lower, "th and ",
      upper, "th percentiles of the non-parametric bootstrap (n=", .n_run, ") estimates."
    )
  )
}
