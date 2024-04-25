#' Combine bootstrap estimates with non-bootstrap estimates and parameter key
#'
#' @description
#'
#' Combines boot strap estimates and non boot strap estimates with information in parameter key. Performs
#' some formatting of this combined data.frame.
#' There are two main steps of this function:
#'
#' 1. Run `bbr::param_estimates_compare` to extract summary quantiles, the 5th, 50th, and 95th, of the
#' bootstrap estimates for each model parameter.
#'
#' Some `parameter_names` have punctuation such as `OMEGA(1,1)`. A new column is
#' added without punctuation, such as `OMEGA11`.
#'
#' Following this, parameter details from the parameter key are joined to the boot strap parameter estimates.
#' A `dplyr::inner_join` is used so that only parameters in the model output are kept
#' in the table. This was done so that, if your base and final model used the same structural
#' THETAs and random parameters, the same parameter key could be used for both.
#'
#' This join adds the following columns: `abb` (abbreviation), `desc` (parameter description),
#' `panel`, `trans` (transformation).
#'
#' 2. Reformat non-bootstrap estimates and left join onto combined bootstrap estimates and parameter key data.frame.
#' Expected input is a data.frame with parameter estimates, with the columns:
#' `parameter_names`, `estimate`.
#'
#' With this information provided, a check is performed to determine whether parameters
#' with special transformation rules were defined correctly. In addition, a series of
#' TRUE/FALSE columns that will be used by subsequent functions.
#'
#' @param .boot_estimates parameter boot estimates- either path to file or data.frame
#' @param .nonboot_estimates non-bootstrap final model - either path to file or bbr model_summary
#' @param .key path to parameter key or data.frame of parameter key. Described in more detail in \link[pmparams]{param_key}
#'
#' @examples
#'
#' boot_paramEst <- utils::read.csv(system.file("model/nonmem/boot/data/boot-106.csv",
#'                                  package = "pmparams"))
#' nonboot_paramEst <- utils::read.csv(system.file("model/nonmem/nonboot_param_est.csv",
#'                                     package = "pmparams"))
#' paramKey <-  system.file("model/nonmem/pk-parameter-key-new.yaml", package = "pmparams")
#'
#' define_boot_table(.boot_estimates = boot_paramEst,
#'                .nonboot_estimates = nonboot_paramEst,
#'                .key = paramKey)
#'
#' @export
define_boot_table <- function(.boot_estimates, .nonboot_estimates, .key){

  #path to boot estimates
  if (inherits(.boot_estimates, "character")){
    .boot <- readr::read_csv(.boot_estimates)
  #data.frame of boot estimates
  } else {
    .boot <- .boot_estimates
  }

# parameter key types
  .key <- loadParamKey(.key)


  .nonboot_estimates <- loadParamEstimates(.nonboot_estimates)
  # #path to nonboot estimates
  # if(inherits(.nonboot_estimates, "character")){
  #   .nonboot_estimates <- bbr::read_model(here::here(.nonboot_estimates)) %>%
  #     bbr::model_summary() %>%
  #     bbr::param_estimates()
  # #data.frame of nonboot estimates
  # } else {
  #   .nonboot_estimates <- .nonboot_estimates
  # }

#clean up boot
.bootParam = .boot %>%
    bbr::param_estimates_compare() %>%
    dplyr::rename(estimate = "50%", lower = "2.5%", upper = "97.5%") %>%
    dplyr::mutate(name = gsub("[[:punct:]]", "", parameter_names)) %>%
    dplyr::inner_join(.key, by = "name")

#join with nonboot estimates
.boot_df <- .bootParam %>%
  dplyr::left_join(.nonboot_estimates %>%
                dplyr::mutate(name = gsub("[[:punct:]]", "", parameter_names)) %>%
                dplyr::select(parameter_names, fixed),
                by = "parameter_names") %>%
  dplyr::mutate(value = estimate) %>%
  checkTransforms() %>%
  defineRows() %>%
  backTrans_log() %>%
  backTrans_logit() %>%
  dplyr::arrange(as.numeric(nrow))

.boot_df

}
