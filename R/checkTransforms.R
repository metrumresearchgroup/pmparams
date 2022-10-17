#' Check whether "~" is used to signify the associated THETA
#'
#' @description
#'
#' Checks to see if there is a '~' in df$trans, replace NA with the value.
#' For example, [logitOmSD ~ THETA1] puts "THETA1" in transTHETA.
#'
#' After, it removes everything after the "~" in the trans column and
#' replaces THETAx in transTHETA with corresponding estimate.
#'
#' Last, replace THETA with corresponding estimate.
#'
#' @param df data frame of parameter estimates
#'
#' @keywords internal
checkTransforms <- function(df){
  df$transTHETA = NA
  if(any(str_detect(df$trans, "~"))){
    df$transTHETA[which(str_detect(df$trans, "~"))] =
      stringr::str_split(df$trans, fixed("~")) %>% map(trimws) %>% map(2) %>% unlist

    df = df %>%
      mutate(trans = case_when(str_detect(trans, "~") ~
                                 stringr::str_split(trans, fixed("~")) %>% map(trimws) %>% map(1) %>% unlist,
                               TRUE ~ trans),
             transTHETA = estimate[match(transTHETA, parameter_names)]
      )
  }
  return(df)
}
