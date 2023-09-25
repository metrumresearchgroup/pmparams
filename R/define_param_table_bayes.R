#' Combine parameter estimates with parameter key for bayes model
#'
#' @description
#'
#' Combines model output parameter estimates with information in parameter key. Performs
#' some formatting of this combined data.frame.
#'
#' @param .estimates data.frame of parameter estimates
#' @param .key path to parameter key or data.frame of parameter key. Described in more detail in \link[pmparams]{param_key}
#' @param .select_param parameters to summarize. Default is all parameters in the parameter key
#' @param .summary_stat summary statistics. Default is median, standard deviation. Potential summary statistics include: mean, median, standard deviation, mad
#' @param .ci confidence interval. Default is 0.95.
#'
#' @export
define_param_table_bayes <- function(.estimates,
                                     .key,
                                     .select_param = "all",
                                     .summary_stat = c("median", "sd"),
                                     .ci = 95){

  # fit0 <- readr::read_rds(here::here("inst", "model", "stan", "mod0", "mod0-output", "fit0_draws.RDS"))
  # .key <- here::here("inst", "model", "stan", "mod0", "mod0-param.yaml")
  # .estimates <- fit0
  # .select_param <- "all"
  # .summary_stat <- c("median", "sd")
  # .ci <- 95

  .key_yaml <- yaml::yaml.load_file(.key)
  .key <- loadParamKey(.key)

  #lower case
  .key <- .key %>%  dplyr::mutate(name = tolower(name))
  names(.key_yaml) <- tolower(names(.key_yaml))
  names(.estimates) <- tolower(names(.estimates))
  .summary_stat <- tolower(.summary_stat)
  .select_param <- tolower(.select_param)

  if (!all(.summary_stat %in% c("mean", "median", "sd", "mad"))){
    stop("Summary statistic provided is not supported by pmarams. See ?define_param_table_bayes for supported summary statistics")
  }

  if (any(.select_param == "all")){
    .select_param <- .key %>% dplyr::filter(.key$name %in% names(.estimates)) %>% dplyr::pull(name)
  } else {
    .select_param <- .key %>% dplyr::filter(.select_param %in% names(.estimates)) %>% dplyr::pull(name)
  }

  if (!all(.key$name %in% names(.estimates))){
    warning(paste0("There are parameters in parameter key or `.select_param` argument that are not in parameters in data.frame of parameter estimates.
                   The following parameters will be dropped: ",
                   .key %>% filter(!(.key$name %in% names(.estimates))) %>% dplyr::pull(name) %>% as.data.frame()
                   ))
  }

  .estimates1 <- .estimates %>%
    dplyr::select(any_of(.select_param)) %>%
    tidyr::pivot_longer(cols = everything()) %>%
    suppressWarnings()

  .estimates2 <- .estimates1 %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(
      mean = mean(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      mad = posterior::mad(value)
    ) %>%
    dplyr::select(name, any_of(.summary_stat)) %>%
    dplyr::distinct() %>%
    dplyr::ungroup()


  fit1 <-  fit0 %>% dplyr::select(any_of(.select_param)) %>% suppressWarnings()

  if (.ci < 1){
    .ci <- .ci
  } else {
    .ci <- .ci/100
  }

  low_ci = (1- .ci)/2
  high_ci = 1-low_ci

  ci_list <- list()

  for (j in .select_param){

    ci_list[[noquote(j)]] <- fit1 %>%
      posterior::extract_variable_matrix(variable = paste0(j)) %>%
      posterior::quantile2(probs = c(low_ci, high_ci)) #check this
  }

  ci_estimates0 <- do.call(cbind, ci_list) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column("name")

  ci_estimates1 <- ci_estimates0[grepl("q",colnames(ci_estimates0))]

  ci_estimates2 <- ci_estimates1 %>%
    tidyr::pivot_longer(everything())  %>%
    dplyr::distinct(name) %>%
    dplyr::mutate(
      num_name = as.numeric(gsub("q", "", name)),
      new_name = dplyr::if_else(num_name == min(num_name), "lower", "upper")
    ) %>%
    dplyr::select(name, new_name) %>%
    tidyr::pivot_wider(names_from = new_name, values_from = name)

  ci_estimates <- ci_estimates0 %>%
    dplyr::rename(lower = ci_estimates2$lower, upper = ci_estimates2$upper)

  mod_estimates <- .estimates2 %>%
    dplyr::left_join(ci_estimates, by = "name") %>%
    dplyr::left_join(.key, by = "name") %>%
    checkTransforms() %>%
    defineRows()

  return(mod_estimates)
}

#scratch- delete
# mod_estimates_auto <- fit0 %>%
#   dplyr::select(all_of(tolower(.select_param))) %>%
#   posterior::summarise_draws() %>%
#   dplyr::rename(name = variable) %>%
#   dplyr::mutate(name = toupper(name)) %>%
#   dplyr::left_join(.key, by = "name") %>%
#   dplyr::mutate(software = .software)
