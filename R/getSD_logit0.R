#' SD and %CV for logit-normal distributions
#'
#' @description
#' CV% not appropriate for logit transforms so report SD
#' %CV for random variable Y.
#'
#' Y = a + b * (1 / (1 + exp(-X)))
#'
#' Where X ~ N(mu, sigma) is the normally-distributed logit term
#' e.g. for PARAM = 1 / (1 + EXP(-(THETA1 + ETA1)))
#'
#' CV equations from \href{https://ascpt.onlinelibrary.wiley.com/doi/full/10.1002/psp4.12404}{NONMEM Tutorial Part I: Description of Commands and Options, With Simple Examples of Population Analysis}
#'
#' @param .mean mean of the logit term
#' @param .var  variance of the logit term
#' @param .a  additive term
#' @param .b  proportional term
#'
#' @export
getSD_logitO <- function(.mean, .var, .a = 0, .b = 1) {

    sdList = NA
  for (i in 1:length(.mean)) {
    m = .mean[i]
    v = .var[i]
    if (is.na(m) | is.na(v)) {
      sd = NA_real_ } else {
        moments <- logitnorm::momentsLogitnorm(mu = m, sigma = sqrt(v))
        sd <- sqrt(moments[["var"]])
      }
    sdList = c(sdList, sd)
  }
  return(sdList[-1])
}
