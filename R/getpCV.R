#' Calculate CV%
#'
#' @export
getpCV <- function(df){
  df %>%
    mutate(cv = case_when(diag & OM & lognormO ~ sig(getCV_lognormO(value)),
                          #diag & OM & logitOsd ~ sig(getSD_logitO(.mean=transTHETA, .var = value)),
                          diag & S & propErr ~ sig(getCV_propS(value)),
                          TRUE ~ "-"))
}
