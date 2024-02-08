#' Make pmtable parameter table
#'
#' @description
#'
#' Generates specific parameter tables by filtering and using pmtables
#'
#' This function:
#' 1. Filters to columns needed for specific parameter tables
#' 2. Panels by "type"
#' 3. Makes "abb", "greek", "desc" blank for "full", "fixed", "fixed structural", "fixed covariate" parameter tables.
#' Makes "abb" and "greek" blank for "fixed random", "random"
#' 4. Attaches notes
#' 5. Rename "value" to "Estimate" and "shrinkage" to "Shrinkage (\\%)", if applicable
#'
#' If these pmtable settings do not work for your parameter table, you can overwrite them afterwards using desired pmtables commands.
#'
#' @param .df parameter data set output from pmparams::format_param_table or pmparams::format_boot_table.
#' @param .pmtype parameter table type. Options include: full, fixed, fixed and random, fixed structural, fixed covariate, random. Defaults to "full".
#' @param .notes footnotes for table. Defaults to NULL.
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
                          .notes = NULL,
                          .width = 1){

.pmtype <- tolower(.pmtype)

.ci_nam <- names(.df)[grepl("ci", names(.df))]
.new_ci_nam <-  paste0(stringr::str_remove(.ci_nam, "ci_"), "\\% CI")

.df0 <- .df %>%
  dplyr::rename(
    !!.new_ci_nam := .ci_nam
  )

if (.pmtype == "full"){
  pm_tab0 <- .df0

} else if (.pmtype == "fixed"){
  pm_tab0 <- .df0 %>%
      dplyr::filter(stringr::str_detect(type, "Struct") |
                    stringr::str_detect(type, "effect"))

} else if (.pmtype == "fixed and random") {
  pm_tab0 <- .df0 %>%
   dplyr::filter(stringr::str_detect(type, "Struct") |
                  stringr::str_detect(type, "effect")) %>%
   dplyr::filter(stringr::str_detect(greek, "Omega") |
                  stringr::str_detect(type, "Resid"))

} else if (.pmtype == "fixed structural") {
  pm_tab0 <- .df0 %>%
    dplyr::filter(stringr::str_detect(type, "Struct"))

} else if (.pmtype == "fixed covariate") {
  pm_tab0 <- .df0 %>%
    dplyr::filter(stringr::str_detect(type, "Covariate"))

} else if (.pmtype == "random") {

  pm_tab0 <- .df0 %>%
    dplyr::filter(stringr::str_detect(greek, "Omega") |
                  stringr::str_detect(type, "Resid"))

} else {
  stop("Incorrect parameter table type. Options for .pmtype are: full, fixed, fixed and random,
       fixed structural, fixed covariate, random. See ?make_pmtable for more details")
}


if (.pmtype %in% c("full", "fixed", "fixed structural", "fixed covariate")){
  pm_tab1 <- pm_tab0 %>%
    dplyr::select(-shrinkage) %>%
    pmtables::st_new() %>%
    pmtables::st_panel("type") %>%
    pmtables::st_blank("abb", "greek", "desc") %>%
    pmtables::st_rename("Estimate" = "value")
} else {
  pm_tab1 <- pm_tab0 %>%
    dplyr::select(-desc) %>%
    pmtables::st_new() %>%
    pmtables::st_panel("type") %>%
    pmtables::st_blank("abb", "greek") %>%
    pmtables::st_rename("Estimate" = "value",
                        "Shrinkage (\\%)" = "shrinkage")
}

if (is.null(.notes)){
  pm_tab2 <- pm_tab1

} else {
  pm_tab2 <- pm_tab1 %>%
        pmtables::st_notes(.notes) %>%
        pmtables::st_notes_detach(width = .width, type = "minipage")
}

return(pm_tab2)

}
