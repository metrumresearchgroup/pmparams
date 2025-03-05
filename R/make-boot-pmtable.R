#' Generate a complete bootstrap parameter table ready for rendering via `pmtables`
#'
#' @details
#'
#' Generates specific parameter tables by filtering and using `pmtables`. This
#' function expects a data.frame with both the regular parameter estimates _and_
#' the bootstrap parameter estimates. See "Examples" for more detail.
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
#' @param .df Combined dataset of model and bootstrap parameter estimates. See
#'  examples.
#' @param .pmtype Parameter table type. Options include:
#' - `"full"` (all rows in `.df` retained in pmtable). This is the default.
#' - `"fixed"` (all rows with type = "Struct" or "effect"),
#' - `"structural"` (all rows with type = "Struct"),
#' - `"covariate"` (all rows with type = "effect"),
#' - `"random"` (all rows with greek = "Omega" or type = "Resid").
#' @param .span_model_label A label for the span above columns relating to the
#'  model that was bootstrapped.
#' @param .span_boot_label A label for the span above columns relating to the
#'  confidence interval of bootstrap estimates.
#' @param .width Notes width. Defaults to 1.
#'
#' @seealso [make_pmtable()], [boot_notes()]
#' @examples
#'
#' \dontrun{
#'
#' model_dir <- system.file("model/nonmem", package = "pmparams")
#' paramKey <-  file.path(model_dir, "pk-parameter-key-new.yaml")
#'
#' # Parameter estimates
#' mod <- bbr::read_model(file.path(model_dir, "106"))
#' param_df <- define_param_table(
#'  .estimates = mod,
#'  .key = paramKey,
#' ) %>% format_param_table()
#'
#' # Bootstrap estimates
#' boot_run <- bbr::read_model(file.path(model_dir, "106-boot"))
#' boot_df <- define_boot_table(
#'  .boot_estimates = bbr::bootstrap_estimates(boot_run),
#'  .key = paramKey
#' ) %>% format_boot_table()
#'
#' # Combine parameter estimates with bootstrap estimates
#' combine_df <- dplyr::left_join(param_df, boot_df)
#'
#' # Fixed effects table
#' make_boot_pmtable(.df = combine_df, .pmtype = "fixed") %>%
#'  pmtables::stable() %>%
#'  # preview in Rstudio viewer (requires `magick` and `pdftools`)
#'  pmtables::st_as_image(border = "0.8cm 0.7cm")
#'
#'
#' # Random effects table
#' make_boot_pmtable(.df = combine_df, .pmtype = "random") %>%
#'  pmtables::stable() %>%
#'  pmtables::st_as_image(border = "0.8cm 0.7cm")
#' }
#' @export
make_boot_pmtable <- function(
    .df,
    .pmtype = c("full", "fixed", "structural", "covariate", "random"),
    .span_model_label = "Full model",
    .span_boot_label = "Non-parametric bootstrap",
    .width = 1
){
  checkmate::assert_numeric(.width)
  .pmtype <- match.arg(.pmtype)

  req_cols <- c("type", "abb", "greek", "desc", "value", "shrinkage")
  if (!all(req_cols %in% names(.df))) {
    missing_cols <- paste(setdiff(req_cols, names(.df)), collapse = ", ")
    stop(glue::glue("The following required columns are missing: {missing_cols}"))
  }

  if (!any(grepl("^boot_", names(.df)))) {
    msg <- paste(
      "No confidence intervals or percentiles for the bootstrap parameter estimates were detected.",
      "See `?make_boot_pmtable` for more details."
    )
    stop(msg)
  }

  # Remove ci_[x] columns
  ci_cols <- names(.df)[grepl("^ci_", names(.df))]
  .df <- .df %>% dplyr::select(-any_of(ci_cols))

  # Rename CI and percent columns
  .df_new <- rename_boot_cols(.df)
  boot_names <- attributes(.df_new)$renamed_cols

  pm_tab0 <-
    if (.pmtype == "full"){
      .df_new
    } else if (.pmtype == "fixed"){
      .df_new %>% dplyr::filter(stringr::str_detect(type, "Struct") | stringr::str_detect(type, "effect"))
    } else if (.pmtype == "structural") {
      .df_new %>% dplyr::filter(stringr::str_detect(type, "Struct"))
    } else if (.pmtype == "covariate") {
      .df_new %>% dplyr::filter(stringr::str_detect(type, "effect"))
    } else if (.pmtype == "random") {
      .df_new %>% dplyr::filter(stringr::str_detect(greek, "Omega") | stringr::str_detect(type, "Resid"))
    }


  # Create pmtable
  pm_tab1 <-
    if (.pmtype == "full"){
      pm_tab0 %>%
        pmtables::st_new() %>%
        pmtables::st_panel("type") %>%
        pmtables::st_blank("abb", "greek", "desc") %>%
        pmtables::st_span(.span_model_label, value:shrinkage) %>%
        pmtables::st_span(.span_boot_label, {{boot_names}})
    } else if (.pmtype %in% c("fixed", "structural", "covariate")){
      pm_tab0 %>%
        dplyr::select(-shrinkage) %>%
        pmtables::st_new() %>%
        pmtables::st_panel("type") %>%
        pmtables::st_blank("abb", "greek", "desc") %>%
        pmtables::st_span(.span_model_label, value) %>%
        pmtables::st_span(.span_boot_label, {{boot_names}})
    } else if (.pmtype == "random"){
      pm_tab0 %>%
        dplyr::select(-desc) %>%
        pmtables::st_new() %>%
        pmtables::st_panel("type") %>%
        pmtables::st_blank("abb", "greek") %>%
        pmtables::st_span(.span_model_label, value:shrinkage) %>%
        pmtables::st_span(.span_boot_label, {{boot_names}})
    }


  pm_tab2 <- pm_tab1 %>%
    pmtables::st_notes_detach(width = .width) %>%
    pmtables::st_rename("Estimate" = "value",
                        "Shrinkage (\\%)" = "shrinkage",
                        "RSE (\\%)" = "pRSE"
    )

  return(pm_tab2)
}

#' Rename and format bootstrap columns
#' @inheritParams make_boot_pmtable
#' @noRd
rename_boot_cols <- function(.df) {
  .df_new <- .df %>%
    dplyr::rename_with(
      .fn = function(rename_col) {
        rename_col %>%
          # Specific renaming
          stringr::str_replace("boot_value", "Median") %>%
          # Value renaming
          stringr::str_replace("boot_ci_([0-9.]+)", "\\1\\\\% CI")
      },
      .cols = dplyr::contains("boot_")
    )

  # Store renamed columns for downstream use
  attr(.df_new, "renamed_cols") <- setdiff(names(.df_new), names(.df))
  return(.df_new)
}
