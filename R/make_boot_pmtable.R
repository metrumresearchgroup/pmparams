#' Make pmtable bootstrap parameter table
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
#' @param .df bootstrap parameter dataset and non-bootstrap parameter dataset combined.
#' @param .pmtype parameter table type. Options include:
#' - `"full"` (all rows in `.df` retained in pmtable). This is the default.
#' - `"fixed"` (all rows with type = "Struct" or "effect"),
#' - `"structural"` (all rows with type = "Struct"),
#' - `"covariate"` (all rows with type = "effect"),
#' - `"random"` (all rows with greek = "Omega" or type = "Resid").
#' @param .show_desc logical (T/F). If `TRUE` (the default), include the description
#'  column in the final table.
#' @param .width notes width. Defaults to 1.
#'
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
#' combine_df <- left_join(param_df, boot_df)
#'
#' # Fixed effects table
#' make_boot_pmtable(.df = combine_df, .pmtype = "fixed") %>%
#'  stable() %>%
#'  st_as_image(border = "0.8cm 0.7cm")
#'
#'
#' # Random effects table
#' make_boot_pmtable(.df = combine_df, .pmtype = "random") %>%
#'  stable() %>%
#'  st_as_image(border = "0.8cm 0.7cm")
#' }
#' @export
make_boot_pmtable <- function(
    .df,
    .pmtype = c("full", "fixed", "structural", "covariate", "random"),
    .show_desc = TRUE,
    .width = 1
){
  checkmate::assert_numeric(.width)
  .pmtype <- match.arg(.pmtype)

  # TODO: improve these checks
  # - Maybe assign classes to the formatted functions
  if(!any(grepl("ci_", names(.df)))){
    message("No parameter estimates detected.")
  }
  if(!any(grepl("perc_", names(.df)))){
    stop("No bootstrap parameter estimates detected.")
  }

  .df_new <- rename_boot_cols(.df)
  boot_names <- attributes(.df_new)$new_columns

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

  # Toggle description inclusion
  if (isFALSE(.show_desc)) pm_tab0 <- pm_tab0 %>% dplyr::select(-desc)

  # Create pmtable
  pm_tab1 <-
    if (.pmtype == "full"){
      pm_tab0 %>%
        pmtables::st_new() %>%
        pmtables::st_panel("type") %>%
        pmtables::st_blank("abb", "greek", "desc") %>%
        pmtables::st_span("Final model", value:shrinkage) %>%
        pmtables::st_span("Non-parametric bootstrap", {{boot_names}})
    } else if (.pmtype %in% c("fixed", "structural", "covariate")){
      pm_tab0 %>%
        dplyr::select(-shrinkage) %>%
        pmtables::st_new() %>%
        pmtables::st_panel("type") %>%
        pmtables::st_blank("abb", "greek", "desc") %>%
        pmtables::st_span("Final model", value) %>%
        pmtables::st_span("Non-parametric bootstrap", {{boot_names}})
    } else if (.pmtype == "random"){
      pm_tab0 %>%
        pmtables::st_new() %>%
        pmtables::st_panel("type") %>%
        pmtables::st_blank("abb", "greek", "desc") %>%
        pmtables::st_span("Final model", value:shrinkage) %>%
        pmtables::st_span("Non-parametric bootstrap", {{boot_names}})
    }


  pm_tab2 <- pm_tab1 %>%
    pmtables::st_notes_detach(width = .width) %>%
    pmtables::st_rename("Estimate" = "value",
                        "Shrinkage (\\%)" = "shrinkage",
                        "RSE (\\%)" = "pRSE"
    )

  return(pm_tab2)
}

rename_boot_cols <- function(.df){
  # Extract bootstrap columns and calculate boot values
  boot_cols_keep <- names(.df)[grepl("perc_", names(.df))]
  boot_values <- as.numeric(gsub("perc_", "", boot_cols_keep))

  cols_keep <- c("type", "abb", "greek", "desc", "value", "shrinkage", boot_cols_keep)
  .df_new <- .df %>% dplyr::select(tidyselect::all_of(cols_keep))

  # This determines which values are paired to another (e.g., 5% and 95%)
  #  - Remove the median (50%) from boot_values before finding CI pairs
  filtered_boot_values <- boot_values[boot_values != 50]
  ci_pairs <- which(filtered_boot_values + rev(filtered_boot_values) == 100)

  new_columns <- c() # Keep track of new columns

  # Create confidence intervals by pairing lower and upper percentiles
  if (length(ci_pairs) > 0 && length(ci_pairs) %% 2 == 0) {
    upper_indices <- length(boot_values) - ci_pairs + 1
    paired_indices <- as.vector(rbind(ci_pairs, upper_indices))[seq_along(ci_pairs)]
    paired_cols <- boot_cols_keep[paired_indices]

    lower_vals <- boot_values[paired_indices[seq(1, length(paired_indices), by = 2)]]
    upper_vals <- boot_values[paired_indices[seq(2, length(paired_indices), by = 2)]]
    ci_names <- paste0(upper_vals - lower_vals, "\\% CI")

    # Add new CI columns to the dataframe
    new_ci_cols <- purrr::map2_dfc(
      split(paired_cols, rep(1:(length(paired_cols)/2), each = 2)),
      ci_names,
      ~ purrr::set_names(
        list(paste(.df_new[[.x[1]]], .df_new[[.x[2]]], sep = " , ")),
        .y
      )
    )

    new_columns <- c(new_columns, names(new_ci_cols))
    .df_new <- dplyr::bind_cols(.df_new, new_ci_cols)
  }

  # Find & handle remaining columns that are not paired to a CI (e.g., median)
  remaining_indices <- setdiff(seq_along(boot_values), paired_indices)
  remaining_cols <- boot_cols_keep[remaining_indices]
  remaining_values <- boot_values[remaining_indices]

  renamed_cols <- .df_new %>%
    dplyr::rename_with(
      .fn = function(remain_col){
        case_when(
          remain_col %in% remaining_cols[remaining_values == 50] ~ "Median",
          TRUE ~ paste0(remaining_values[match(remain_col, remaining_cols)], "\\%")
        )},
      .cols = all_of(remaining_cols)
    )

  new_columns <- c(new_columns, setdiff(names(renamed_cols), names(.df_new)))

  # Remove any old columns that still exist
  .df_final <- renamed_cols %>% dplyr::select(-any_of(boot_cols_keep))
  attr(.df_final, "new_columns") <- new_columns

  return(.df_final)
}
