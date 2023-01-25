newDF2 <- newDF %>%
  getCI(.value = "estimate", .se = "stderr")

newDF3 <- newDF2 %>%
  formatParamTable()

newDF4 <- newDF2 %>%
  formatParamTable(.prse = TRUE)

newDF5 <- newDF2 %>%
  formatParamTable(.select_cols = "all")

newDF6 <- newDF2 %>%
  formatParamTable(.select_cols = "all", .prse = TRUE)


test_that("formatParamTable expected dataframe: col names and row number [MPT-FPT-001]", {
  #default cols., no prse
  expect_equal(names(newDF3),  c("type", "abb", "greek", "desc", "value", "ci", "shrinkage"))
  expect_equal(nrow(newDF3), nrow(newDF2))

  #all cols., no prse
  expect_equal(length(newDF5),  36)
  expect_equal(nrow(newDF5), nrow(newDF2))
})

test_that("formatParamTable expected dataframe: prse col[MPT-FPT-002]", {
  #default cols., prse
  expect_equal(length(newDF4),  length(newDF3) + 1)
  expect_true("pRSE" %in% names(newDF6))
  expect_equal(nrow(newDF4), nrow(newDF2))

  expect_equal(length(newDF6),  length(newDF5) + 1)
  expect_true("pRSE" %in% names(newDF6))
  expect_equal(nrow(newDF5), nrow(newDF2))
})


test_that("formatParamTable continuous columns expected ouput: CI back transforms [MPT-FPT-003]", {
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

test_that("formatParamTable continuous columns expected ouput: shrinkage [MPT-FPT-003]", {
  newDF2_shrink <- newDF2 %>%
    dplyr::mutate(
      shrinkage = if_else(is.na(shrinkage), "-",
                          as.character(pmtables::sig(shrinkage))
                          )
    )

  expect_equal(newDF3$shrinkage[1], newDF2_shrink$shrinkage[1])
  expect_equal(newDF3$shrinkage[6], newDF2_shrink$shrinkage[6])

})

test_that("formatParamTable continuous columns expected ouput: value [MPT-FPT-004]", {
  expect_equal(newDF3$value[1], "1.54")
  expect_equal(newDF3$value[6], "0.221 [CV\\%=49.7]")
})
