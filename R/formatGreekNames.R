#' Format THETA/OMEGA/SIGMA values
#'
#' @description
#' Format THETA/OMEGA/SIGMA values to display as greek letters with subscript numbers.
#'
#' @keywords internal
formatGreekNames <- function(.df){

  .df %>%
    dplyr::mutate(greekName = name) %>%
    # make column with greek letters and parameter numbers
    tidyr::separate(
      greekName,
      into = c("text", "num"),
      sep = "(?<=[A-Za-z])(?=[0-9])"
    ) %>%
    tidyr::separate(
      parameter_names,
      into = c("text2", "num2"),
      sep = "A"
    ) %>%
    dplyr::select(-num, -text2) %>%
    dplyr::mutate(
      text = dplyr::case_when(
        OM ~ "Omega",
        S ~ "Sigma",
        TRUE ~ tolower(text)),
      greek = dplyr::case_when(
        TH & LOG ~ expGreek(text, num2),
        TH & LOGIT ~ logitGreek(text, num2),
        TRUE ~ mathMode(greekNum(gtGreek(text), num2))
        )
    )
}
