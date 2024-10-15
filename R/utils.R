#' Checks if all packages needed are present
#'
#' Returns a vector with the missing packages, or returns `NULL` if all are
#' present.
#' @noRd
check_for_suggested_pkgs <- function(pkgs = "bbr") {
  pkgs_present <- purrr::map_lgl(pkgs, function(.pkg) {
    requireNamespace(.pkg, quietly = TRUE)
  })

  if (any(!pkgs_present)) {
    return(pkgs[!pkgs_present])
  } else {
    return(NULL)
  }
}


#' Skip tests if missing suggested packages
#' @noRd
skip_if_missing_deps <- function(pkgs = "bbr") {
  missing_pkgs <- check_for_suggested_pkgs(pkgs = pkgs)
  testthat::skip_if(
    !is.null(missing_pkgs),
    glue::glue("Skipped because the following packages are needed for this test: {paste(missing_pkgs, collapse = ', ')}")
  )
}

#' Error if missing suggested packages
#' @noRd
stop_if_missing_deps <- function(pkgs = "bbr") {
  missing_pkgs <- check_for_suggested_pkgs(pkgs = pkgs)
  if (!is.null(missing_pkgs)) {
    rlang::abort(paste(
      glue::glue("The following suggested packages needed for this functionality are not installed: {paste(missing_pkgs, collapse = ', ')}"),
      "Consider running `install.packages('pmparams', dependencies = TRUE)`",
      "\nIf using pkgr, add the following to your pkgr.yml and re-run `pkgr install`",
      "\nCustomizations:\n  Packages:\n    - pmparams:\n        Suggests: true",
      sep = "\n"
    ), call. = FALSE)
  }
}
