#' Combine bootstrap estimates with parameter key
#'
#' @description
#'
#' Combines boot strap estimates and non boot strap estimates with information in parameter key. Performs
#' some formatting of this combined data.frame.
#' There are two main steps of this function:
#'
#'
#'#TODO: Update to percentiles
#'1.Run `bbr::param_estimates_compare` to extract summary quantiles, the 5th, 50th, and 95th, of the
#' bootstrap estimates for each model parameter.
#'
#' - Some `parameter_names` have punctuation such as `OMEGA(1,1)`. A new column is
#' added without punctuation, such as `OMEGA11`.
#'
#' - Following this, parameter details from the parameter key are joined to the boot strap parameter estimates.
#' A `dplyr::inner_join` is used so that only parameters in the model output are kept
#' in the table. This was done so that, if your base and final model used the same structural
#' THETAs and random parameters, the same parameter key could be used for both.
#'
#' - This join adds the following columns: `abb` (abbreviation), `desc` (parameter description),
#' `panel`, `trans` (transformation).
#'
#' With this information provided, a check is performed to determine whether parameters
#' with special transformation rules were defined correctly. In addition, a series of
#' TRUE/FALSE columns that will be used by subsequent functions.
#'
#' @param .boot_estimates parameter boot estimates- either path to file or data.frame
#' @param .key path to parameter key or data.frame of parameter key. Described in more detail in \code{\link[pmparams]{param_key}}
#' @param .ci specify 90 or 95 percent confidence interval or IQR (interquartile range). Default is 95.
#' @param .percentiles list of all percentiles. Default is NULL. For .ci = 95, .percentiles = c(0.025, 0.5, 0.975). For .ci =90, .percentiles will be c(0.05, 0.5, 0.95)
#' @param .na.rm Default is false
#' @examples
#'
#' boot_paramEst <- utils::read.csv(system.file("model/nonmem/boot/data/boot-106.csv",
#'                                  package = "pmparams"))
#' paramKey <-  system.file("model/nonmem/pk-parameter-key-new.yaml", package = "pmparams")
#'
#' define_boot_table(.boot_estimates = boot_paramEst,
#'                .key = paramKey)
#'
#' @export
define_boot_table <- function(.boot_estimates, .key, .ci = 95, .percentiles = NULL, .na.rm = FALSE){

  # .boot_estimates = boot_paramEstPath
  # .ci = 95
  # .key = paramKeyPath
  # .percentile = c(0.7, 0.5, 0.3, 0.65)

  #path to boot estimates
  if (inherits(.boot_estimates, "character")){
    .boot <- readr::read_csv(.boot_estimates, show_col_types = FALSE)
  #data.frame of boot estimates
  } else {
    .boot <- .boot_estimates
  }

# parameter key types
  .key <- loadParamKey(.key)

#clean up boot
  .bootParam0 = .boot %>%
    getBootPercentiles(.ci, .percentiles, .na.rm)

.bootParam <- .bootParam0 %>%
    dplyr::mutate(name = gsub("[[:punct:]]", "", parameter_names)) %>%
    dplyr::inner_join(.key, by = "name")

#join with nonboot estimates
.boot_df <- .bootParam %>%
  checkTransforms() %>%
  defineRows() %>%
  backTrans_log() %>%
  backTrans_logit() %>%
  dplyr::arrange(as.numeric(nrow))

.boot_df

}
