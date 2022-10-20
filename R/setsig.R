#' Set significant digits
#'
#' @export
setsig <- function(.dig, .maxex = NULL) {
  options(mrgparamtab.dig = .dig)
  options(mrgparamtab.maxex = .maxex)
}
