#' Combine bootstrap estimates with parameter key
#'
#' @description
#'
#' Combines bootstrap estimates with information in parameter key and performs
#' some additional formatting.
#'
#' @details
#'
#' Below is the expected format of `.boot_estimates` if a data frame is provided:
#'    ```
#'    run   THETA1 THETA2 THETA3 THETA4 THETA5 THETA6   THETA7 THETA8
#'    <chr>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>    <dbl>  <dbl>
#'    001    0.494   4.19   1.20   4.20   1.24  0.488 -0.0615   0.377
#'    002    0.405   4.07   1.14   4.23   1.32  0.536 -0.103    0.517
#'    003    0.500   4.09   1.15   4.21   1.27  0.413 -0.0752   0.523
#'    004    0.578   4.16   1.20   4.18   1.30  0.518 -0.0502   0.418
#'    005    0.499   4.14   1.16   4.25   1.22  0.436 -0.0686   0.394
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
#'
#' @param .boot_estimates One of the following:
#' - Output from `bbr::bootstrap_estimates()` or a wide data frame denoting the
#' parameter estimates for each run. See details.
#' - A file path to a csv containing the above dataset.
#' @param .key path to parameter key or data.frame of parameter key. Described
#'   in more detail in \code{\link[pmparams]{param_key}}
#' @inheritParams getBootPercentiles
#' @examples
#'
#' model_dir <- system.file("model/nonmem", package = "pmparams")
#' paramKey <-  file.path(model_dir, "pk-parameter-key-new.yaml")
#'
#' # Using a file path:
#' boot_path <- file.path(model_dir, "boot/data/boot-106.csv")
#' define_boot_table(
#'  .boot_estimates = boot_path,
#'  .key = paramKey
#' )
#'
#' # Using a `bbr` bootstrap model object:
#' \dontrun{
#' boot_run <- bbr::read_model(file.path(model_dir, "106-boot"))
#' define_boot_table(
#'  .boot_estimates = bbr::bootstrap_estimates(boot_run),
#'  .key = paramKey
#' )
#' }
#'
#' @seealso [param_key()], [define_param_table()]
#' @export
define_boot_table <- function(
    .boot_estimates,
    .key,
    .ci = 95,
    .percentiles = NULL,
    .na.rm = TRUE
){
  # path to boot estimates
  if (inherits(.boot_estimates, "character")){
    .boot <- readr::read_csv(.boot_estimates, show_col_types = FALSE)
    # data.frame of boot estimates
  } else {
    .boot <- .boot_estimates
  }

  # parameter key types
  .key <- loadParamKey(.key)

  # get percentiles and format
  .bootParam0 <- .boot %>% getBootPercentiles(.ci, .percentiles, .na.rm)

  .boot_df <- .bootParam0 %>%
    removePunc(.column = "parameter_names") %>%
    dplyr::inner_join(.key, by = "name") %>%
    checkTransforms() %>%
    defineRows() %>%
    backTrans_log() %>%
    backTrans_logit() %>%
    dplyr::arrange(as.numeric(nrow)) %>%
    tibble::as_tibble()

  return(.boot_df)
}
