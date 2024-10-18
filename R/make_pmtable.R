#' Make pmtable parameter table
#'
#' @details
#'
#' Generates specific parameter tables by filtering and using `pmtables`
#'
#' This function:
#' 1. Filters to columns needed for specific parameter tables
#' 2. Panels by "type"
#' 3. Makes `"abb"`, `"greek"`, `"desc"` blank (no title)
#'     - Note that description is removed when `.pmtype = "random"`. See
#'       `?pmtables::st_mutate()` if you want to add it back in.
#' 4. Attaches notes
#' 5. Rename "value" to "Estimate" and "shrinkage" to "Shrinkage (%)", if
#'    applicable
#'
#' **Note:**
#' If these `pmtables` settings do not work for your parameter table, you can
#' overwrite them afterwards using desired `pmtables` commands.
#'
#' @param .df parameter data set output from pmparams::format_param_table.
#' @param .pmtype parameter table type. Options include:
#' - `"full"` (all rows in `.df` retained in pmtable). This is the default.
#' - `"fixed"` (all rows with type = "Struct" or "effect"),
#' - `"structural"` (all rows with type = "Struct"),
#' - `"covariate"` (all rows with type = "effect"),
#' - `"random"` (all rows with greek = "Omega" or type = "Resid").
#' @param .width notes width. Defaults to 1.
#'
#' @examples
#'
#' model_dir <- system.file("model/nonmem", package = "pmparams")
#' paramKey <-  file.path(model_dir, "pk-parameter-key-new.yaml")
#'
#' # Read in parameter estimates (or provide `bbr` model)
#' param_est <- utils::read.csv(file.path(model_dir, "param_est.csv"))
#'
#' # Create formatted parameter table
#' param_df <- define_param_table(
#'  .estimates = param_est,
#'  .key = paramKey
#' ) %>% format_param_table()
#'
#' \dontrun{
#'
#' # Create random effects table that can be rendered to latex
#' make_pmtable(param_df, .pmtype = "random") %>%
#'  pmtables::stable() %>%
#'  # preview in Rstudio viewer (requires `magick` and `pdftools`)
#'  pmtables::st_as_image()
#'}
#'
#' @seealso [make_boot_pmtable()]
#' @export
make_pmtable <- function(
    .df,
    .pmtype = c("full", "fixed", "structural", "covariate", "random"),
    .width = 1
){
  checkmate::assert_numeric(.width)
  .pmtype <- match.arg(.pmtype)

  # Rename CI columns
  .df0 <- .df
  .ci_nam <- names(.df)[grepl("ci_", names(.df))]
  .new_ci_nam <-  paste0(stringr::str_remove(.ci_nam, "ci_"), "\\% CI")
  names(.df0)[which(names(.df0) == .ci_nam)] <- .new_ci_nam

  pm_tab0 <-
    if (.pmtype == "full"){
      .df0
    } else if (.pmtype == "fixed"){
      .df0 %>% dplyr::filter(stringr::str_detect(type, "Struct") | stringr::str_detect(type, "effect"))
    } else if (.pmtype == "structural") {
      .df0 %>% dplyr::filter(stringr::str_detect(type, "Struct"))
    } else if (.pmtype == "covariate") {
      .df0 %>% dplyr::filter(stringr::str_detect(type, "effect"))
    } else if (.pmtype == "random") {
      .df0 %>% dplyr::filter(stringr::str_detect(greek, "Omega") | stringr::str_detect(type, "Resid"))
    }

  # Create pmtable
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
