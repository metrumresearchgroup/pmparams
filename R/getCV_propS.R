#' Get CV proportion
#'
#' @description
#' CV equations from \href{https://ascpt.onlinelibrary.wiley.com/doi/full/10.1002/psp4.12404}{NONMEM Tutorial Part I: Description of Commands and Options, With Simple Examples of Population Analysis}
#'
#' @keywords internal
getCV_propS  <- function(v){
  sqrt(v) * 100
}
