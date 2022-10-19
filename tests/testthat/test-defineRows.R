#defineRows

test_that("defineRows unexpected input: trans column is not in list of valid trans values [MPT-DRO-001]",{
  paramKey2 <- paramKey
  paramKey2$trans[1] <- "lgoTrans"
  expect_error(capture.output(defineRows(paramKey2)))
})


test_that("defineRows expected output: generates logical columns to indicate trans type [MPT-DRO-002]", {
  newDF <- defineRows(paramKey)
  expect_equal(all(newDF$LOG[newDF$trans == "logTrans"]), TRUE)
  expect_equal(any(newDF$LOG[newDF$trans == "logitTrans"]), FALSE)
  expect_equal(all(newDF$LOGIT[newDF$trans == "logitTrans"]), TRUE)
  expect_equal(any(newDF$LOGIT[newDF$trans == "logTrans"]), FALSE)
})

test_that("defineRows expected output: generates logical columns to indicate name type [MPT-DRO-002]", {
  expect_equal(all(newDF$TH[newDF$name == "THETA1"]), TRUE)
  expect_equal(all(newDF$TH[newDF$name == "SIGMA11"]), FALSE)
  expect_equal(all(newDF$S[newDF$name == "SIGMA11"]), TRUE)
  expect_equal(all(newDF$S[newDF$name == "THETA1"]), FALSE)
})


