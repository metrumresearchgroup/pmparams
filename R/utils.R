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

#' Check if specified package is above a certain version
#' @examples
#' \dontrun{
#'  version_above("bbr", "1.11.0")
#' }
#' @noRd
version_above <- function(pkg, version){
  above <- utils::packageVersion(pkg) >= package_version(version)
  above <- stats::setNames(above, paste(pkg, version))
  return(above)
}

#' Skip tests if missing _suggested_ packages.
#' @examples
#' \dontrun{
#'  skip_if_missing_deps("bbr")
#'
#'  # Provide version requirement
#'  skip_if_missing_deps("bbr", "1.11.0")
#'
#'  # Multiple packages/versions
#'  skip_if_missing_deps(c("bbr", "withr"), c("1.11.0", "3.0.1"))
#' }
#' @noRd
skip_if_missing_deps <- function(pkgs = "bbr", vers = NULL) {
  missing_pkgs <- check_for_suggested_pkgs(pkgs = pkgs)
  testthat::skip_if(
    !is.null(missing_pkgs),
    glue::glue("The following packages are needed for this test: {paste(missing_pkgs, collapse = ', ')}")
  )

  if (!is.null(vers)) {
    checkmate::assert_true(length(pkgs) == length(vers))
    pkg_vers <- purrr::map2(pkgs, vers, function(.pkg, .ver) {
      version_above(.pkg, .ver)
    }) %>% unlist()

    testthat::skip_if(
      any(!pkg_vers),
      glue::glue("The following package _versions_ are needed for this test: {paste(names(pkg_vers[!pkg_vers]), collapse = ', ')}")
    )
  }
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


#' Suppress a warning that matches `.regexpr`
#' @param .expr Expression to run
#' @param .regexpr Regex to match against any generated warning. Warning will be
#'  suppressed if this matches the warning message.
#' @noRd
suppressSpecificWarning <- function(.expr, .regexpr) {
  withCallingHandlers({
    .expr
  }, warning=function(w) {
    if (stringr::str_detect(w$message, .regexpr))
      invokeRestart("muffleWarning")
  })
}
