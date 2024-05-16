#' Make pmtable parameter table
#'
#' @description
#'
#' Generates specific parameter tables by filtering and using pmtables
#'
#' This function:
#' 1. Filters to columns needed for specific parameter tables
#' 2. Panels by "type"
#' 3. Makes "abb", "greek", "desc" blank for "full", "fixed", "structural", "covariate" parameter tables.
#' Makes "abb" and "greek" blank for "random"
#' 4. Attaches notes
#' 5. Rename "value" to "Estimate" and "shrinkage" to "Shrinkage (\\%)", if applicable
#'
#' If these pmtable settings do not work for your parameter table, you can overwrite them afterwards using desired pmtables commands.
#'
#' @param .df parameter data set output from pmparams::format_param_table or pmparams::format_boot_table.
#' @param .pmtype parameter table type. Options include: full (all rows in .df retained in pmtable), fixed (all rows with type = "Struct" or "effect"), structural (all rows with type = "Struct"), covariate (all rows with type = "effect"), random (all rows with greek = "Omega" or type = "Resid"). Defaults to "full".
#' @param .width notes width. Defaults to 1.
#'
#' @examples
#'
#' #Using output from `format_param_table` (defineOut),
#' paramEst <- utils::read.csv(system.file("model/nonmem/param_est.csv", package = "pmparams"))
#' paramKey <-  system.file("model/nonmem/pk-parameter-key-new.yaml", package = "pmparams")
#' defineOut <- define_param_table(.estimates = paramEst, .key = paramKey, .ci = 95, .zscore = NULL)
#' data <- format_param_table(.df = defineOut)
#'
#' #To make random effects table:
#'
#' make_pmtable(.df = data, .pmtype = "random")
#' @export
make_pmtable <- function(.df,
                         .pmtype = "full",
                         .width = 1){

  .pmtype <- tolower(.pmtype)

  .ci_nam <- names(.df)[grepl("ci", names(.df))]
  .new_ci_nam <-  paste0(stringr::str_remove(.ci_nam, "ci_"), "\\% CI")

  .df0 <- .df

  names(.df0)[which(names(.df0) == .ci_nam)] <- .new_ci_nam

  pm_tab0 <-
    if (.pmtype == "full"){
      .df0

    } else if (.pmtype == "fixed"){
      .df0 %>%
        dplyr::filter(stringr::str_detect(type, "Struct") |
                        stringr::str_detect(type, "effect"))
    } else if (.pmtype == "structural") {
      .df0 %>%
        dplyr::filter(stringr::str_detect(type, "Struct"))

    } else if (.pmtype == "covariate") {
      .df0 %>%
        dplyr::filter(stringr::str_detect(type, "effect"))

    } else if (.pmtype == "random") {

      .df0 %>%
        dplyr::filter(stringr::str_detect(greek, "Omega") |
                        stringr::str_detect(type, "Resid"))

    } else {
      stop("Incorrect parameter table type. Options for .pmtype are: full, fixed, structural, covariate, random. See ?make_pmtable for more details")
    }

  pm_tab1 <-

    if (.pmtype == "full"){
      pm_tab0 %>%
        pmtables::st_new() %>%
        pmtables::st_panel("type") %>%
        pmtables::st_blank("abb", "greek", "desc")
    } else if (.pmtype %in% c("fixed", "structural", "covariate")){
        pm_tab0 %>%
          dplyr::select(-shrinkage) %>%
          pmtables::st_new() %>%
          pmtables::st_panel("type") %>%
          pmtables::st_blank("abb", "greek", "desc")
    } else if (.pmtype == "random"){
      pm_tab0 %>%
        dplyr::select(-desc) %>%
        pmtables::st_new() %>%
        pmtables::st_panel("type") %>%
        pmtables::st_blank("abb", "greek")
    }


  pm_tab2 <- pm_tab1 %>%
    pmtables::st_notes_detach(width = .width) %>%
    pmtables::st_rename("Estimate" = "value",
                        "Shrinkage (\\%)" = "shrinkage",
                        "RSE (\\%)" = "pRSE")

  return(pm_tab2)

}
