#' Load parameter key
#'
#' @description
#' Loads parameter key by providing parameter key path  or data frame of parameter key.
#' Checks that parameter key contains the following columns: name, abb, desc, panel, trans.
#'
#' @param .key path to parameter key or data.frame of parameter key
#'
#' @keywords internal
loadParamKey <- function(.key){

  if (inherits(.key, "character")){
    print(paste0("Parameter table yaml path provided: ", .key))
    y1l <- yaml::yaml.load_file(.key)

    if (!all(names(y1l[[1]]) %in% c("abb", "desc", "panel", "trans"))) {
      warning("Only abb, desc, panel and trans arguments will be used, all others ignored")
    }

    .key <- dplyr::tibble(
      name = names(y1l),
      abb = unlist(y1l)[grepl('abb',names(unlist(y1l)),fixed=T)],
      desc = unlist(y1l)[grepl('desc',names(unlist(y1l)),fixed=T)],
      panel = unlist(y1l)[grepl('panel',names(unlist(y1l)),fixed=T)],
      trans = unlist(y1l)[grepl('trans',names(unlist(y1l)),fixed=T)]
    )
  }

  if (inherits(.key, "data.frame")){
    if (!(all(c("name", "abb", "desc", "panel", "trans") %in% colnames(.key)))) {
      stop("Incorrect parameter key input type. See ?param_key for list of valid parameter key inputs")
    }
  } else{
    stop("Incorrect parameter key input type. See ?param_key for list of valid parameter key inputs")
  }

  .key
}
