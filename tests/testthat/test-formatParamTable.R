newDF2 <- newDF %>%
  getCI(.value = "estimate", .se = "stderr")

newDF3 <- newDF2 %>%
  formatParamTable()

test_that("formatParamTable expected dataframe: col names and row number [MPT-FPT-001]", {
  expect_equal(names(newDF3),  c("type", "abb", "greek", "desc", "value", "ci", "shrinkage"))
  expect_equal(nrow(newDF3), nrow(newDF2))
})

test_that("formatParamTable categorical columns expected output: type [MPT-FPT-002]", {

})

# test_that("formatParamTable categorical columns expected output: abb [MPT-FPT-002]", {
#   expect_equal(newDF3$abb[1], newDF2$abb[1])
#   expect_equal(newDF3$abb[7], paste0(newDF2$panel[7], "-", newDF2$abb[7]))
# })

test_that("formatParamTable categorical columns expected output: greek [MPT-FPT-002]", {

})

test_that("formatParamTable categorical columns expected output: desc [MPT-FPT-002]", {

})

test_that("formatParamTable continuous columns expected ouput: CI back transforms", {
  newDF2_log1 <- newDF2 %>%
                dplyr::mutate(
                      value = dplyr::case_when(LOG ~ exp(value), TRUE ~ value),
                      lower = dplyr::case_when(LOG ~ exp(lower), TRUE ~ lower),
                      upper = dplyr::case_when(LOG ~ exp(upper), TRUE ~ upper)
                ) %>%
                dplyr::mutate(
                  ci = paste0(pmtables::sig(lower), ", ", pmtables::sig(upper))
                )

  expect_equal(newDF3$ci[1], newDF2_log1$ci[1])


})

test_that("formatParamTable continuous columns expected ouput: shrinkage", {
  newDF2_shrink <- newDF2 %>%
    dplyr::mutate(
      shrinkage = if_else(is.na(shrinkage), "-",
                          as.character(pmtables::sig(shrinkage))
                          )
    )

  expect_equal(newDF3$shrinkage[1], newDF2_shrink$shrinkage[1])
  expect_equal(newDF3$shrinkage[6], newDF2_shrink$shrinkage[6])

})

test_that("formatParamTable continuous columns expected ouput: value", {
  expect_equal(newDF3$value[1], "1.54")
  expect_equal(newDF3$value[6], "0.221 [CV\\%=49.7]")
})
