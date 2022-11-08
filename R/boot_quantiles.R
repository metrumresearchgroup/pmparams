#' Calculate quartiles when using bootstrap estimates
#'
#' @param x numeric vector whose sample quantiles are wanted
#' @param prob numeric vector of probabilities with values in [0,1]
#'
#' @keywords internal
#' @rdname qt
qt   <- function(.x, .prob){
  unname(quantile(.x, probs = .prob, na.rm = TRUE))
}

#' @rdname qt
#' @keywords internal
med  <- function(.x){
  qt(.x, 0.5)
}

#' @rdname qt
#' @keywords internal
lo  <- function(.x){
  qt(.x, 0.025)
}

#' @rdname qt
#' @keywords internal
hi  <- function(.x){
  qt(.x, 0.975)
}
