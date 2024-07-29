
test_that("getValueSE accurately calculates each parameter's accociated metric", {
  param_df1 <- getValueSE(.df = newDF)
  expect_equal(param_df1$value, newDF$estimate)
  expect_equal(param_df1$se, newDF$stderr)

  param_df2 <- param_df1 %>% filter(OM == TRUE & diag != TRUE | S == TRUE & diag == TRUE & addErr == TRUE) %>% select(corr_SD)
  expect_equal(param_df2$corr_SD, c(0.510933, 0.693796, 0.621554))

  param_df3 <-  param_df1$corr_SD[!(param_df1$corr_SD %in% param_df2$corr_SD)]
  expect_true(is.na(unique(param_df3)))

})

test_that("getValueSE digits and maxex options for each parameter's accociated metric", {
  param_df2 <- getValueSE(.df = newDF)
  expect_equal(nchar(param_df2$corr_SD[9]), 8)
})
