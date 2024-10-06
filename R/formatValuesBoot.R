#' Define how values are to be displayed
#'
#' @description
#' Format values for bootstrap run
#'
#' @keywords internal

formatValuesBoot <- function(.boot_df,
                             .digit,
                             .maxex){

  perc_nam <- names(.boot_df)[grepl("perc", names(.boot_df))]

  if (any(grepl("lower_perc", names(.boot_df)))){

    perc_nam1 <- as.numeric(gsub('.*perc', '', perc_nam))
    perc <- perc_nam1[3] - perc_nam1[1]
    perc_value <- perc_nam1[2]

    if (all(perc_nam1 == c( 25.2, 50.0 ,75.0))){
      perc = "iqr"
    }

    .boot_df %>%
      dplyr::mutate(
        "boot_perc_{{perc}}" := paste0(pmtables::sig(!!dplyr::sym(perc_nam[1]), .digit, .maxex),
                                       ', ',
                                       pmtables::sig(!!dplyr::sym(perc_nam[3]), .digit, .maxex)
                                       ),
        "boot_value_{{perc_value}}" := pmtables::sig(!!dplyr::sym(perc_nam[2]), .digit, .maxex)
      )
  } else {
    .boot_df %>%
      dplyr::mutate_at(perc_nam, ~pmtables::sig(., .digit, .maxex))
  }

}
