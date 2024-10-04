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
#' @param .boot_df bootstrap parameter data set output from pmparams::format_boot_table.
#' @param .nonboot_df non-boot data set. If provided, this will be joined onto the bootstrap parameter estimates. Default is NULL.
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
#' #To make random effects table:
#'
#' make_boot_pmtable(.boot_df = data, .pmtype = "random")
#'
#' #To include non-bootstrap estimates:
#'
#' paramPath <- system.file("model/nonmem/102", package = "pmparams")
#' defineOut_nonboot <- define_param_table(.estimates = paramPath, .key = paramKey, .ci = 95, .zscore = NULL)
#' data_nonboot <- format_param_table(defineOut_nonboot)
#'
#' combine <- data_nonboot %>% left_join(data)
#' make_boot_pmtable(.boot_df = combine, .pmtype = "random")
#'
#' @export
make_boot_pmtable <- function(.boot_df,
                              .pmtype = "full",
                              .width = 1,
                              .nonboot_df = NULL){

  .pmtype <- tolower(.pmtype)

  if (any(grepl("ci_", names(.boot_df)))){
    .nonboot_ci <- names(.boot_df)[grepl("ci_", names(.boot_df))]
    .nonboot_ci_nam  <-  paste0(stringr::str_remove(.nonboot_ci, "ci_"), "\\% CI")
  }

  #rename boot value and ci (only if n=3 percentiles given)
  #case if boot multiple percentiles, return all, do not reformat
  #rename boot value and ci
  .boot_perc <- names(.boot_df)[grepl("perc_", names(.boot_df))]
  .boot_value <- names(.boot_df)[grepl("boot_value", names(.boot_df))]

  #rename boot percentile, else keep names as is
  if (length(.boot_perc) == 1){
    .boot_perc_nam  <-  paste0(stringr::str_remove(.boot_perc, "boot_perc_"), "\\% CI")
    .boot_value_nam  <-  dplyr::if_else(paste0(stringr::str_remove(.boot_value, "boot_value_")) == "50", "Median", "\\%") #TODO: Confirm median rename with Katherine
  }

  #names(.combine)[which(names(.combine) == .ci_nam)] <- .new_ci_nam

  pm_tab0 <-
    if (.pmtype == "full"){
      .boot_df

    } else if (.pmtype == "fixed"){
      .boot_df %>%
        dplyr::filter(stringr::str_detect(type, "Struct") |
                        stringr::str_detect(type, "effect"))
    } else if (.pmtype == "structural") {
      .boot_df %>%
        dplyr::filter(stringr::str_detect(type, "Struct"))

    } else if (.pmtype == "covariate") {
      .boot_df %>%
        dplyr::filter(stringr::str_detect(type, "effect"))

    } else if (.pmtype == "random") {

      .boot_df %>%
        dplyr::filter(stringr::str_detect(greek, "Omega") |
                        stringr::str_detect(type, "Resid"))

    } else {
      stop("Incorrect parameter table type. Options for .pmtype are: full, fixed, structural, covariate, random. See ?make_pmtable for more details")
    }


  ## add cases for if have nonboot estimates
  if(!any(grepl("ci_", names(.boot_df)))){
  pm_tab1 <-

    if (.pmtype == "full"){
      pm_tab0 %>%
        pmtables::st_new() %>%
        pmtables::st_panel("type") %>%
        pmtables::st_blank("abb", "greek", "desc") %>%
        pmtables::st_span("Non-parametric bootstrap", {{.boot_value}}:{{.boot_perc}})

    } else if (.pmtype %in% c("fixed", "structural", "covariate")){
        pm_tab0 %>%
          #dplyr::select(-shrinkage) %>% #TODO: bootstrap can't have shrinkage? OK to drop
          pmtables::st_new() %>%
          pmtables::st_panel("type") %>%
          pmtables::st_blank("abb", "greek", "desc") %>%
          pmtables::st_span("Non-parametric bootstrap", {{.boot_value}}:{{.boot_perc}})
    } else if (.pmtype == "random"){
      pm_tab0 %>%
        dplyr::select(-desc) %>%
        pmtables::st_new() %>%
        pmtables::st_panel("type") %>%
        pmtables::st_blank("abb", "greek") %>%
        pmtables::st_span("Non-parametric bootstrap", {{.boot_value}}:{{.boot_perc}})
    }


  pm_tab2 <- pm_tab1 %>%
    pmtables::st_notes_detach(width = .width) %>%
    pmtables::st_rename("Estimate" = "value",
                        "RSE (\\%)" = "pRSE",
                        {{.boot_value_nam}} := {{.boot_value}},
                        {{.boot_perc_nam}} := {{.boot_perc}}
                        )
  } else {
    pm_tab1 <-

      if (.pmtype == "full"){
        pm_tab0 %>%
          pmtables::st_new() %>%
          pmtables::st_panel("type") %>%
          pmtables::st_blank("abb", "greek", "desc") %>%
          pmtables::st_span("Final model", value:shrinkage) %>% #TODO add check that will put all these next to eachother (do select statement)
          pmtables::st_span("Non-parametric bootstrap", {{.boot_value}}:{{.boot_perc}})

      } else if (.pmtype %in% c("fixed", "structural", "covariate")){
        pm_tab0 %>%
          dplyr::select(-shrinkage) %>%
          pmtables::st_new() %>%
          pmtables::st_panel("type") %>%
          pmtables::st_blank("abb", "greek", "desc") %>%
          pmtables::st_span("Final model", value:{{.nonboot_ci}}) %>%
          pmtables::st_span("Non-parametric bootstrap", {{.boot_value}}:{{.boot_perc}})
      } else if (.pmtype == "random"){
        pm_tab0 %>%
          dplyr::select(-desc) %>%
          pmtables::st_new() %>%
          pmtables::st_panel("type") %>%
          pmtables::st_blank("abb", "greek") %>%
          pmtables::st_span("Final model", value:{{.nonboot_ci}}) %>%
          pmtables::st_span("Non-parametric bootstrap", {{.boot_value}}:{{.boot_perc}})
      }


    pm_tab2 <- pm_tab1 %>%
      pmtables::st_notes_detach(width = .width) %>%
      pmtables::st_rename("Estimate" = "value",
                          "Shrinkage (\\%)" = "shrinkage",
                          "RSE (\\%)" = "pRSE",
                          {{.nonboot_ci_nam}} := {{.nonboot_ci}},
                          {{.boot_value_nam}} := {{.boot_value}},
                          {{.boot_perc_nam}} := {{.boot_perc}}
      )
  }

  return(pm_tab2)

}
