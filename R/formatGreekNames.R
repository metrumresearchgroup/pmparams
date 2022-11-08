#' Format THETA/OMEGA/SIGMA values
#'
#' @description
#' Format THETA/OMEGA/SIGMA values to display as greek letters with subscript numbers.
#'
#' @export
formatGreekNames <- function(.df){
  .df %>%
    mutate(greekName = name) %>%
    # make column with greek letters and parameter numbers
    separate(greekName,
             into = c("text", "num"),
             sep = "(?<=[A-Za-z])(?=[0-9])"
    ) %>%
    separate(parameter_names,
             into = c("text2", "num2"),
             sep = "A"
    ) %>%
    select(-num, -text2) %>%
    mutate(text = case_when(OM ~ "Omega",
                            S ~ "Sigma",
                            TRUE ~ tolower(text)),
           greek = case_when(TH & LOG ~ expGreek(text, num2),
                             TH & LOGIT ~ logitGreek(text, num2),
                             TRUE ~ mathMode(greekNum(gtGreek(text), num2))
           )
    )
}
