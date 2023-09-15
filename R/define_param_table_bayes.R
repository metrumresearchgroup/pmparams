#' Combine parameter estimates with parameter key
#'
#' @description
#'
#' Combines model output parameter estimates with information in parameter key. Performs
#' some formatting of this combined data.frame.
#'
#' Expected input is a data.frame with parameter estimates, with the columns:
#' `parameter_names`, `estimate`, `stderr`, `random_effect_sd`, `random_effect_sdse`,
#' `fixed`, `diag`, `shrinkage`.
#'
#' Some `parameter_names` have punctuation such as `OMEGA(1,1)`. A new column is
#' added without punctuation, such as `OMEGA11`.
#'
#' Following this, parameter details from the parameter key are joined to the parameter estimates.
#' A `dplyr::inner_join` is used so that only parameters in the model output are kept
#' in the table. This was done so that, if your base and final model used the same structural
#' THETAs and random parameters, the same parameter key could be used for both.
#'
#' This join adds the following columns: `abb` (abbreviation), `desc` (parameter description),
#' `panel`, `trans` (transformation).
#'
#' With this information provided, a check is performed to determine whether parameters
#' with special transformation rules were defined correctly. In addition, a series of
#' TRUE/FALSE columns that will be used by subsequent functions.
#'
#' @param .estimates data.frame of parameter estimates
#' @param .key path to parameter key or data.frame of parameter key. Described in more detail in \link[pmparams]{param_key}
#' @param .select_param parameters to summarize. Default is all parameters in the parameter key
#' @param .summary_stat summary statistics. Default is median, standard deviation, 2.5% and 97.5% CI
#' @param .software type of software used to. Default is stan
#'
#' @examples
#'
#' paramEst <- utils::read.csv(system.file("model/nonmem/param_est.csv", package = "pmparams"))
#' paramKey <-  system.file("model/nonmem/pk-parameter-key-new.yaml", package = "pmparams")
#'
#' define_param_table(.estimates = paramEst, .key = paramKey, .ci = 95, .zscore = NULL)
#'
#' #To choose a confidence interval that is not 95 or 90, look up z-score and add as function parameter
#' define_param_table(.estimates = paramEst, .key = paramKey, .ci = 82, .zscore = 0.915)
#'
#' @export
define_param_table_bayes <- function(.estimates, .key, .select_param = "all", .summary_stat, .software = "stan"){

  fit0 <- readr::read_rds(here::here("inst", "model", "stan", "mod0", "mod0-output", "fit0_draws.RDS"))
  .key <- here::here("inst", "model", "stan", "mod0", "mod0-param.yaml")
  .estimates <- fit0
  y1l <- yaml::yaml.load_file(.key)
  .key <- loadParamKey(.key)
  .select_param <- c("emax", "sigma")
  .summary_stat <- c("median", "sd", "ci2.5", "ci97.5")

  names(y1l) <- toupper(names(y1l))
  names(.estimates) <- toupper(names(.estimates))

  if (.select_param == "all"){ #warning message- fix
    .select_param <- names(y1l)
  } else {
    .select_param <- toupper(.select_param)
  }

  .estimates1<- .estimates %>%
    dplyr::select(all_of(.select_param)) #warning message- fix

  mod_estimates <- .estimates1 %>%
    mutate_all(across(.select_param), mean = mean())
  #removePunc(.column = "parameter_names") %>%
  dplyr::inner_join(.key, by = "name") %>%
    checkTransforms() %>%
    defineRows() %>%
    getValueSE() %>%
    getCI(.ci = .ci, .zscore = .zscore)


  .summary_stat1 <-
    .estimates1 %>%
    sapply(median) %>%
    as.data.frame()

  # want this at thend
  # A tibble: 2 × 12
  # variable mean  median sd    mad   q5    q95   rhat  ess_bulk ess_tail abb           desc
  # <chr>    <chr> <chr>  <chr> <chr> <chr> <chr> <chr>    <num>    <num> <chr>         <chr>
  #   1 emax     98.6  98.8   1.08  1.02  96.5  99.9  1.00     3065.    2082. "$\\theta_3$" Maximal tooth length
  # 2 sigma    10.4  10.4   0.226 0.221 10.0  10.7  1.00     8538.    2761. "$\\sigma$"   Residual standard deviation
  return(mod_estimates)
}
