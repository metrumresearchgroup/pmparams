#' Make pmtable boot parameter table
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
#' @param .df bootstrap parameter data set output from pmparams::format_boot_table or bootstrap parameter dataset and non-bootstrap parameter dataset combined.
#' @param .pmtype parameter table type. Options include: full (all rows in .df retained in pmtable), fixed (all rows with type =
#' "Struct" or "effect"), structural (all rows with type = "Struct"), covariate (all rows with type = "effect"), random (all
#' rows with greek = "Omega" or type = "Resid"). Defaults to "full".
#' @param .width notes width. Defaults to 1.
#'
#' @examples
#'
#' #Using output from `format_boot_table` (defineOut),
#' bootEst <- utils::read.csv(system.file("model/nonmem/boot/data/boot-106.csv", package = "pmparams"))
#' paramKey <-  system.file("model/nonmem/pk-parameter-key-new.yaml", package = "pmparams")
#' defineOut <- define_boot_table(.boot_estimates = bootEst, .key = paramKey, .ci = 95)
#' data <- format_boot_table(.boot_df = defineOut)
#'
#'
#' #To make random effects table with bootstrap and non-bootstrap data:
#'
#' paramPath <- system.file("model/nonmem/102", package = "pmparams")
#' defineOut_nonboot <- define_param_table(.estimates = paramPath,
#'                                         .key = paramKey)
#' data_nonboot <- format_param_table(defineOut_nonboot)
#'
#' combine <- dplyr::left_join(data_nonboot, data)
#' make_boot_pmtable(.df = combine, .pmtype = "random")
#'
#' @export
make_boot_pmtable <- function(.df,
                              .pmtype = "full",
                              .width = 1){

  .pmtype <- tolower(.pmtype)

  if (any(grepl("ci_", names(.df))) & any(grepl("perc", names(.df)))){
    message("Bootstrap and non-bootstrap parameter estimates detected.")
  } else if (!any(grepl("ci_", names(.df))) & any(grepl("boot_perc_", names(.df)))){
    stop("Only bootstrap parameter estimates detected.")
    #.df <- .df %>% dplyr::mutate(diag = TRUE)
    #.df <- .df %>% getPanelName()
    #.df <- .df %>% dplyr::mutate(type = panel)
  } else {
    stop("No bootstrap parameter estimates detected.")
  }

  .df <- .df %>%
    dplyr::select(type, abb, greek, desc, value, shrinkage, names(.df)[grepl("boot", names(.df))])

  #rename boot percentile, else keep names as is
  if (length(names(.df)[grepl("perc_", names(.df))]) == 1){
    .boot_perc <- names(.df)[grepl("perc_", names(.df))]
    .boot_value <- names(.df)[grepl("boot_value", names(.df))]

    .boot_perc_nam  <-  paste0(stringr::str_remove(.boot_perc, "boot_perc_"), "\\% CI")
    .boot_value_nam  <-  dplyr::if_else(paste0(stringr::str_remove(.boot_value, "boot_value_")) == "50", "Median", "\\%") #TODO: Confirm median rename with Katherine
  } else {
    .perc <- names(.df)[grepl("perc", names(.df))]
    .boot_value <- .perc[1]
    .boot_perc <- .perc[length(.perc)]

   # names(.df)[grepl("perc", names(.df))] <- paste0(stringr::str_remove(names(.df)[grepl("perc", names(.df))] , "perc"), "\\%")
  }

  pm_tab0 <-
  if (.pmtype == "full"){
    .df
  } else if (.pmtype == "fixed"){
    .df %>%
      dplyr::filter(stringr::str_detect(type, "Struct") |
                  stringr::str_detect(type, "effect"))
  } else if (.pmtype == "structural") {
    .df %>%
        dplyr::filter(stringr::str_detect(type, "Struct"))
  } else if (.pmtype == "covariate") {
    .df %>%
        dplyr::filter(stringr::str_detect(type, "effect"))
  } else if (.pmtype == "random") {
    .df %>%
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
          pmtables::st_blank("abb", "greek", "desc") %>%
          pmtables::st_span("Final model", value:shrinkage) %>%
          pmtables::st_span("Non-parametric bootstrap", {{.boot_value}}:{{.boot_perc}})

      } else if (.pmtype %in% c("fixed", "structural", "covariate")){
        pm_tab0 %>%
          dplyr::select(-shrinkage) %>%
          pmtables::st_new() %>%
          pmtables::st_panel("type") %>%
          pmtables::st_blank("abb", "greek", "desc") %>%
          pmtables::st_span("Final model", value) %>%
          pmtables::st_span("Non-parametric bootstrap", {{.boot_value}}:{{.boot_perc}})
      } else if (.pmtype == "random"){
        pm_tab0 %>%
          dplyr::select(-desc) %>%
          pmtables::st_new() %>%
          pmtables::st_panel("type") %>%
          pmtables::st_blank("abb", "greek") %>%
          pmtables::st_span("Final model", value:shrinkage) %>%
          pmtables::st_span("Non-parametric bootstrap", {{.boot_value}}:{{.boot_perc}})
      }


    pm_tab2 <- pm_tab1 %>%
      pmtables::st_notes_detach(width = .width) %>%
      pmtables::st_rename("Estimate" = "value",
                          "Shrinkage (\\%)" = "shrinkage",
                          "RSE (\\%)" = "pRSE"
      )

    pm_tab3 <-
      if (length(.boot_value_nam) > 0){
        pm_tab2 %>%
            pmtables::st_rename(
                  {{.boot_value_nam}} := {{.boot_value}},
                  {{.boot_perc_nam}} := {{.boot_perc}}
            )
      } else {
        pm_tab2
      }

  return(pm_tab3)

}
