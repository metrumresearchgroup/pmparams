#' Format parameter table
#'
#' @description
#'
#' ADD HERE
#'
#' Expected input is a data.frame with parameter estimates, with the columns:
#'
#' What it does:
#' select_cols = argument for deciding which columns are present in final data frame
#' Default for select_cols is c(type, abb, greek, desc, value, ci, shrinkage)
#' If select_cols = NULL, return ALL columns present
#' Another optional arugment select_cols_additional for adding other columns to select
#'
#'
#' @param .estimates parameter estimates data.frame
#' @param .key parameter key
#' @param ... arguments passed through to methods (currently undefined)
#'
#' @seealso \link[mrgparamtab]{param_key}: Parameter key requirements
#' @export



# format_param_table <- function(.df, .select_cols = c(type, abb, greek, desc, value, ci, shrinkage)){
#
#  paramtab1 <-  .df %>%
#     format_values() %>%
#     formatGreekNames() %>%
#     getPanelName()
#
#  if (.select_cols = NULL) {
#    paramtab2 <- paramtab1 %>% as.data.frame()
#  } else {
#    paramtab2 <- paramtab1 %>%
#      dplyr::select(.select_cols) %>%
#      as.data.frame
#  }
#
#  if (.select_cols_additional & .select_cols != NULL){
#    paramtab2 <- paramtab2 %>%
#      dplyr::select(.select_cols_additional)
#  } else if (.select_cols_additional & .select_cols == NULL){
#    paramtab2 <- paramtab2
#  } else {
#    message("cannot do this ")
#  }
#
#   paramtab2
# }
