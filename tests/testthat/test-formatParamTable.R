newDF3 <- newDF %>%
  formatParamTable()

newDF4 <- newDF %>%
  formatParamTable(.prse = TRUE)

newDF5 <- newDF %>%
  formatParamTable(.select_cols = "all")

newDF6 <- newDF %>%
  formatParamTable(.select_cols = "all", .prse = TRUE)


test_that("formatParamTable expected dataframe: col names [MPT-FPT-001]", {
  #default cols., no prse
  expect_equal(names(newDF3),  c("type", "abb", "greek", "desc", "value", "ci", "shrinkage"))

  #all cols., no prse
  expect_equal(length(names(newDF5)),  38)
})

test_that("formatParamTable expected dataframe: prse col[MPT-FPT-002]", {
  #default cols., prse
  expect_equal(newDF4$pRSE[1],  "6.29")
  expect_true("pRSE" %in% names(newDF6))

  expect_equal(length(names(newDF6)),  length(names(newDF5)))
  expect_true("pRSE" %in% names(newDF6))
})


test_that("formatParamTable continuous columns expected ouput: CI back transforms [MPT-FPT-003]", {
  newDF_log1 <- newDF %>%
    dplyr::mutate(
      ci = paste0(pmtables::sig(lower), ", ", pmtables::sig(upper))
    )

  expect_equal(newDF_log1$ci[5], "1.22, 1.36")
})

test_that("formatParamTable continuous columns expected ouput: shrinkage [MPT-FPT-003]", {
  newDF_shrink <- newDF %>%
    dplyr::mutate(
      shrinkage = if_else(is.na(shrinkage), "-",
                          as.character(pmtables::sig(shrinkage))
      )
    )

  expect_equal(newDF_shrink$shrinkage[1], "-")
  expect_equal(newDF_shrink$shrinkage[6], "17.9")
  expect_equal(newDF_shrink$shrinkage[8], "6.02")
})

test_that("formatParamTable continuous columns expected ouput: value [MPT-FPT-004]", {
  expect_equal(newDF3$value[1], "1.54")
  expect_equal(newDF3$value[6], "0.221 [CV\\%=49.7]")
})

