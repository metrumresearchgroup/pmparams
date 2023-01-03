#' Greek number helper functions
#' @keywords internal
mathMode <- function(.x){
  glue::glue("$<<.x>>$", .open = "<<", .close  = ">>")
}

#' @rdname mathMode
#' @keywords internal
gtGreek <- function(.x){
  glue::glue("\\<<.x>>", .open = "<<", .close  = ">>")
}

#' @rdname mathMode
#' @keywords internal
greekNum <- function(.x, .y){
  glue::glue("<<.x>>_{<<.y>>}", .open = "<<", .close  = ">>")
}

#' @rdname mathMode
#' @keywords internal
expGreek  <- function(.x, .y){
  glue::glue("$\\exp(\\<<.x>>_{<<.y>>})$", .open = "<<", .close  = ">>")
}

#' @rdname mathMode
#' @keywords internal
logitGreek  <- function(.x, .y){
  glue::glue("$\\exp(\\<<.x>>_{<<.y>>}) / \\newline(1 + \\exp(\\<<.x>>_{<<.y>>}))$", .open = "<<", .close  = ">>")
}

