#' Combine parameter estimates with parameter key
#'
#' @description
#'
#' Combines model output parameter estimates with information in parameter key. Performs
#' some formatting of this combined data.frame.
#'
#' @details
#'
#' Below is the expected format of `.estimates` if a data frame is provided:
#'    ```
#'    parameter_names estimate  stderr random_effect_sd random_effect_sdse fixed diag  shrinkage
#'    <chr>              <dbl>   <dbl>            <dbl>              <dbl> <lgl> <lgl>     <dbl>
#'    THETA1            0.443  0.0643            NA               NA       FALSE NA       NA
#'    THETA2            4.12   0.0275            NA               NA       FALSE NA       NA
#'    THETA3            1.17   0.0280            NA               NA       FALSE NA       NA
#'    THETA4            4.21   0.0190            NA               NA       FALSE NA       NA
#'    THETA5            1.28   0.0348            NA               NA       FALSE NA       NA
#'    THETA6            0.485  0.0395            NA               NA       FALSE NA       NA
#'    THETA7           -0.0378 0.0635            NA               NA       FALSE NA       NA
#'    THETA8            0.419  0.0863            NA               NA       FALSE NA       NA
#'    ```
#'
#' **Notes:**
#' - Some parameter names may have punctuation (such as `OMEGA(1,1)`). A new
#' `name` column is automatically added that specifies the name without
#'  punctuation (i.e. `OMEGA11`).
#'
#' - Parameter details from the parameter key are joined to the bootstrap parameter
#' estimates. A `dplyr::inner_join` is used so that only parameters in the model
#' output are kept in the table. This was done so that, if your base and final
#' model used the same structural THETAs and random parameters, the same
#' parameter key could be used for both.
#'   - This join adds the following columns: `abb` (abbreviation), `desc`
#'     (parameter description), `panel`, `trans` (transformation).
#'
#' - A final check is performed to determine whether parameters with special
#' transformation rules were defined correctly. In addition, a series of `TRUE`/
#' `FALSE` columns are added that will be used downstream.
#'
#' @inheritParams loadParamEstimates
#' @param .key path to parameter key or data.frame of parameter key. Described
#'   in more detail in \code{\link[pmparams]{param_key}}
#' @param .ci confidence interval. Default is 95
#' @param .zscore z-score for the specified confidence interval. Only needed for
#'   confidence intervals that are **not** 90 or 95 percent
#'
#' @examples
#'
#' model_dir <- system.file("model/nonmem", package = "pmparams")
#' paramKey <-  file.path(model_dir, "pk-parameter-key-new.yaml")
#'
#' # Using a `bbr` model
#' mod <- bbr::read_model(file.path(model_dir, "106"))
#' define_param_table(
#'  .estimates = mod,
#'  .key = paramKey,
#'  .ci = 95,
#' )
#'
#' # Using a file path
#' param_est_path <- file.path(model_dir, "param_est.csv")
#' param_ests <- readr::read_csv(param_est_path)
#' define_param_table(
#'  .estimates = param_ests,
#'  .key = paramKey,
#'  .ci = 95,
#' )
#'
#' # To choose a confidence interval that is not 95 or 90, look up and provide the z-score
#' define_param_table(
#'  .estimates = param_ests,
#'  .key = paramKey,
#'  .ci = 82,
#'  .zscore = 0.915
#' )
#'
#' @seealso [param_key()], [define_boot_table()]
#' @export
define_param_table <- function(.estimates, .key, .ci = 95, .zscore = NULL){


  .estimates <- loadParamEstimates(.estimates)

  .key <- loadParamKey(.key)

  mod_estimates <- .estimates %>%
    removePunc(.column = "parameter_names") %>%
    dplyr::inner_join(.key, by = "name") %>%
    checkTransforms() %>%
    defineRows() %>%
    getValueSE() %>%
    getCI(.ci = .ci, .zscore = .zscore) %>%
    dplyr::arrange(as.numeric(nrow))

  if(any(mod_estimates$THETAERR)){
    message("THETA term was used in $ERROR:  ", paste(mod_estimates$parameter_names[mod_estimates$THETAERR], collapse = ", "))
  }

  return(mod_estimates)
}
