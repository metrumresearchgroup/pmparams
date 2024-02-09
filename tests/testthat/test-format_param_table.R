newDF3 <- newDF %>%
  format_param_table()

newDF4 <- newDF %>%
  format_param_table(.prse = TRUE)

newDF5 <- newDF %>%
  format_param_table(.select_cols = "all")

newDF6 <- theta_err_df1 %>%
  format_param_table(.select_cols = "all")

test_that("format_param_table expected dataframe: col names", {
  #default cols., no prse
  expect_equal(names(newDF3),  c("type", "abb", "greek", "desc", "value", "ci", "shrinkage"))

  #all cols., no prse
  expect_equal(length(names(newDF5)),  40)
})

test_that("format_param_table expected dataframe: prse col", {
  #default cols., prse
  expect_equal(newDF4$pRSE[1],  "6.29")
  expect_true("pRSE" %in% names(newDF6))

  expect_equal(length(names(newDF6)),  length(names(newDF5)))
  expect_true("pRSE" %in% names(newDF6))
})


test_that("format_param_table continuous columns expected ouput: CI back transforms]", {
  newDF_log1 <- newDF %>%
    dplyr::mutate(
      ci = paste0(pmtables::sig(lower), ", ", pmtables::sig(upper))
    )

  expect_equal(newDF_log1$ci[5], "1.22, 1.36")
})

test_that("format_param_table continuous columns expected ouput: shrinkage", {
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

test_that("format_param_table continuous columns expected ouput: value", {
  expect_equal(newDF3$value[1], "1.54")
  expect_equal(newDF3$value[6], "0.221 [CV\\%=49.7]")
})

test_that("format_param_table continuous columns expected ouput: greek", {
  expect_equal(newDF5$greek[newDF5$S & !newDF5$THETAERR], "$\\Sigma_{(1,1)}$")
  expect_equal(newDF6$greek[newDF6$S & newDF6$THETAERR], "$\\theta_{(1,1)}$")
})

