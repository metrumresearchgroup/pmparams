pathDF <- defineParamTable(.est = param_est, .key = paramKey)

test_that("getValueSE accurately calculates each parameter's accociated metric [MPT-GVS-001]", {
  param_df1 <- getValueSE(.df = pathDF, .digit = 3, .maxex = NULL)
  expect_equal(param_df1$value, pathDF$estimate)
  expect_equal(param_df1$se, pathDF$stderr)

  param_df2 <- param_df1 %>% filter(OM == TRUE & diag != TRUE | S == TRUE & diag == TRUE & addErr == TRUE) %>% select(corr_SD)
  expect_equal(param_df2$corr_SD, c("0.511", "0.694", "0.622"))

  param_df3 <-  param_df1$corr_SD[!(param_df1$corr_SD %in% param_df2$corr_SD)]
  expect_equal(unique(param_df3), "-")

})

test_that("getValueSE digits and maxex options for each parameter's accociated metric  [MPT-GVS-003]", {
  param_df2 <- getValueSE(.df = pathDF, .digit = 3, .maxex = NULL)
  expect_equal(nchar(param_df2$corr_SD[9]), 5)
})
