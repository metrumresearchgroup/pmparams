#' Combine bootstrap estimates with parameter key
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
#' @param .boot_estimates parameter boot estimates data.frame- maybe add additional input options
#' @param .nonboot_estimates non-bootstrap final model - either path to file or model_summary
#' @param .key parameter key
#'
#' @seealso \link[mrgparamtab]{param_key}: Parameter key requirements
#' @export
defineBootTable <- function(.boot_estimates, .nonboot_estimates, .key){

  #input options:
  #option 1: csv generated from boot-collect.R
  if (inherits(.boot_estimates, "character")){
    .boot <- readr::read_csv(.boot_estimates)
  } else {
    .boot <- .boot_estimates
  }

# parameter key types
  if (inherits(.key, "character")){
    print(paste0("Parameter table yaml path provided: ", .key))
    y1l <- yaml::yaml.load_file(.key)

    if (!all(names(y1l[[1]]) %in% c("abb", "desc", "panel", "trans"))) {
      warning("Only abb, desc, panel and trans arguments will be used, all others ignored")
    }

    .key <- dplyr::tibble(
      name = names(y1l),
      abb = unlist(y1l)[grepl('abb',names(unlist(y1l)),fixed=T)],
      desc = unlist(y1l)[grepl('desc',names(unlist(y1l)),fixed=T)],
      panel = unlist(y1l)[grepl('panel',names(unlist(y1l)),fixed=T)],
      trans = unlist(y1l)[grepl('trans',names(unlist(y1l)),fixed=T)]
    )
  }

  if (inherits(.key, "data.frame")){
    if (!(all(c("name", "abb", "desc", "panel", "trans") %in% colnames(.key)))) {
      stop("Incorrect parameter key input type. See ?param_key for list of valid parameter key inputs")
    }
  } else{
    stop("Incorrect parameter key input type. See ?param_key for list of valid parameter key inputs")
  }

  if(inherits(.nonboot_estimates, "character")){
    .nonboot_estimates <- bbr::read_model(here::here(nonboot_param_est_path)) %>%
      bbr::model_summary() %>%
      bbr::param_estimates()
  } else {
    .nonboot_estimates <- .nonboot_estimates
  }

#clean up boot
.bootParam = .boot %>%
    bbr::param_estimates_compare() %>%
    dplyr::rename(estimate = "50%", lower = "2.5%", upper = "97.5%") %>%
    dplyr::mutate(name = gsub("[[:punct:]]", "", parameter_names)) %>%
  dplyr::inner_join(.key, by = "name")

#join with key
.boot_df <- .bootParam %>%
  dplyr::left_join(.nonboot_estimates %>%
                #param_estimates() %>%
                dplyr::mutate(name = gsub("[[:punct:]]", "", parameter_names)) %>% select(parameter_names, fixed),
                by = "parameter_names") %>%
  dplyr::mutate(value = estimate) %>%
  checkTransforms() %>%
  defineRows() %>%
  backTrans_log() %>%
  backTrans_logit()

.boot_df
#return(.boot_df)

}
