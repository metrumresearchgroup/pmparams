#' Calculate % RSE
#'
#' @description
#' Note, this is appropriate when parameters are estimated untransformed or in the log.
#' It may not be appropriate if any other transformations (such as logit) were used
#'
#' @export
getpRSE <- function(df){
  df %>%
    mutate(pRSE = case_when(fixed ~ "-",
                            # pRSE of a log-trans TH is equivalent to the CV% of a log-trans TH
                            TH & LOG ~ sig  (sqrt(exp(se^2)-1)*100),
                            TH & !LOG & !LOGIT ~ sig ((se/abs(value)) * 100),
                            diag & !LOG & !LOGIT ~ sig ((se/abs(value)) * 100),
                            TRUE ~ "-"))
}
