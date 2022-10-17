#' Add parentheses around values
#'
#' @param .x value to add parentheses around
#'
#' @keywords internal
parens <- function(.x){
  paste0('(',.x,')')
}

#' @rdname parens
#' @keywords internal
parensSQ <- function(.x){
  paste0('[',.x,']')
}

#' @rdname parens
#' @keywords internal
parensSQ_CV <- function(.x){
  glue::glue("[CV\\%=<<.x>>]", .open = "<<", .close  = ">>")
}

#' @rdname parens
#' @keywords internal
parensSQ_corr <- function(.x){
  glue::glue("[Corr=<<.x>>]", .open = "<<", .close  = ">>")
}

#' @rdname parens
#' @keywords internal
parensSQ_se <- function(.x){
  glue::glue("[SD=<<.x>>]", .open = "<<", .close  = ">>")
}

