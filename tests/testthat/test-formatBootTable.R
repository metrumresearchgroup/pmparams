newDF3 <- newbootDF %>%
  formatBootTable()

newDF5 <- newbootDF %>%
  formatBootTable(.select_cols = "all")


test_that("formatBootTable expected dataframe: col names [MPT-FBT-001]", {
  #default cols
  expect_equal(names(newDF3),  c("abb", "desc", "boot_value", "boot_ci"))

  #all cols
  expect_equal(length(names(newDF5)),  24)
})


# test_that("formatParamTable continuous columns expected ouput: CI back transforms [MPT-FPT-003]", {
#   newDF_log1 <- newDF3 %>%
#     dplyr::mutate(
#       ci = paste0(pmtables::sig(lower), ", ", pmtables::sig(upper))
#     )
#
#   expect_equal(newDF_log1$ci[5], "1.22, 1.36")
# })

# test_that("formatParamTable continuous columns expected ouput: shrinkage [MPT-FPT-003]", {
#   newDF_shrink <- newDF %>%
#     dplyr::mutate(
#       shrinkage = if_else(is.na(shrinkage), "-",
#                           as.character(pmtables::sig(shrinkage))
#       )
#     )
#
#   expect_equal(newDF_shrink$shrinkage[1], "-")
#   expect_equal(newDF_shrink$shrinkage[6], "17.9")
#   expect_equal(newDF_shrink$shrinkage[8], "6.02")
# })

# test_that("formatParamTable continuous columns expected ouput: value [MPT-FPT-004]", {
#   expect_equal(newDF3$value[1], "1.54")
#   expect_equal(newDF3$value[6], "0.221 [CV\\%=49.7]")
# })

