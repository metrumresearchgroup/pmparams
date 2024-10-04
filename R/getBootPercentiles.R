#' Calculate percentiles
#'
#' @description
#'
#' Calculates the upper and lower bound of a 90% or 95% confidence interval, interquartile range, or any number of percentiles based on the
#' value and standard error.
#'
#' @param .df data.frame with parameter estimates
#' @param .ci specify IQR, 90 or 95 confidence intervals. Default is 95.
#' @param .percentiles list of all percentiles. Default is NULL. For .ci = 95, .percentiles = c(0.025, 0.5, 0.975). For .ci =90, .percentiles will be c(0.05, 0.5, 0.95)
#' @param .na.rm Default is false
#'
#' @keywords internal
#'
getBootPercentiles <- function(.boot, .ci, .percentiles, .na.rm) {

  comp_df <- .boot %>%
    dplyr::select(dplyr::starts_with(c("THETA", "SIGMA", "OMEGA")))

  quantile_fn <- function(x)  {
    stats::quantile(x, probs = .percentiles, na.rm = .na.rm)
  }


#check inputs for .ci and .percentile
  .ci = as.character(.ci)
  .ci = tolower(.ci)

  #.ci must be numeric and only 1 number between 0 and 100
  if (length(.ci) > 1){
    stop("`.ci` can only be 1 number")
  }

  #convert iqr
  if (.ci %in% c(50, "50", "iqr")){
    .ci = 50
  } else if (.ci %in% c(90, "90", 95, "95")){
    .ci = as.numeric(.ci)
  } else {
    message("Invalid `.ci` input. define_boot_table` only supports `.ci`` = 90, 95, IQR.
          Only .percentiles argument will be used.")
  }

  if (.ci == 95 & length(.percentiles) == 0){
    .percentiles = c(0.025, .5, 0.975)
  } else if (.ci == 90){
    .percentiles = c(0.05, 0.5, 0.95)
  } else if (.ci == 50){
    .percentiles = c(.25, 0.5, 0.75)
  } else {
    .percentiles = .percentiles
    .ci = NULL
  }

  if (length(.percentiles) == 0){
    stop("Please provide `.ci` or `.percentile` argument")
  }

  if (typeof(.percentiles) != "double"){
    stop("Invalid `.percentiles` input. Must be a numeric list.")
  }

.percentiles <- .percentiles[order(.percentiles)]

  if(any(.percentiles > 1)){
    stop("`.percentiles` provided are outside of [0,1] range")
  }

  if (length(.percentiles) == 3){
    .percentiles <- .percentiles[order(.percentiles)]
    .ci <- .percentiles[3] -.percentiles[1]
  } else {
    .ci = NULL
  }

  comp_df <- comp_df %>%
    dplyr::reframe(dplyr::across(.cols = dplyr::everything(), .fns = quantile_fn)) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column()

  if (length(.percentiles) == 3){
    colnames(comp_df) <- c("parameter_names",
                           paste0("lower_perc", .percentiles[1]*100),
                           paste0("value_perc", .percentiles[2]*100),
                           paste0("upper_perc", .percentiles[3]*100))
  } else {
    .percentiles_nam <- paste0("perc", .percentiles*100)
    colnames(comp_df) <- c("parameter_names", .percentiles_nam )
  }

  return(comp_df)
}
