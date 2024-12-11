#defineRows

test_that("defineRows unexpected input: trans column is not in list of valid trans values",{
  paramKey2 <- PARAM_KEY_DF
  paramKey2$trans[1] <- "lgoTrans"
  expect_error(capture.output(defineRows(paramKey2)))
})


test_that("defineRows expected output: generates logical columns to indicate trans type", {
  newDF <- defineRows(PARAM_KEY_DF)
  expect_equal(all(newDF$LOG[newDF$trans == "logTrans"]), TRUE)
  expect_equal(any(newDF$LOG[newDF$trans == "logitTrans"]), FALSE)
  expect_equal(all(newDF$LOGIT[newDF$trans == "logitTrans"]), TRUE)
  expect_equal(any(newDF$LOGIT[newDF$trans == "logTrans"]), FALSE)
})

test_that("defineRows expected output: generates logical columns to indicate name type", {
  expect_equal(all(PARAM_TAB_102$TH[PARAM_TAB_102$name == "THETA1"]), TRUE)
  expect_equal(all(PARAM_TAB_102$TH[PARAM_TAB_102$name == "SIGMA11"]), FALSE)
  expect_equal(all(PARAM_TAB_102$S[PARAM_TAB_102$name == "SIGMA11"]), TRUE)
  expect_equal(all(PARAM_TAB_102$S[PARAM_TAB_102$name == "THETA1"]), FALSE)
})

test_that("defineRows expected output: theta error block", {
  expect_equal(unique(theta_err_df1$THETAERR[theta_err_df1$TH == TRUE]), FALSE)
  expect_equal(theta_err_df1$S[theta_err_df1$THETAERR == TRUE], TRUE)
  expect_equal(theta_err_df1$S[theta_err_df1$panel == "RV"], TRUE)
})


