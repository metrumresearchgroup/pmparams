#' Calculate 95% confidence intervals
#'
#' @export
get95CI <- function(df){
  df %>%
    mutate(lower = lowerCI(value, se),
           upper = upperCI(value, se))
}
