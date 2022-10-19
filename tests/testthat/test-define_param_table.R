
test_that("define_param_table expected output: creates new parameter names without parentheses [MPT-DPT-001]", {
  expect_equal(newDF$name[newDF$parameter_names == "OMEGA(1,1)"], "OMEGA11")
})

test_that("define_param_table expected output: generates logical columns to indicate parameter type [MPT-DPT-001]", {
  expect_equal(newDF$TH[newDF$parameter_names == "THETA1"], TRUE)
  expect_equal(newDF$OM[newDF$parameter_names == "THETA1"], FALSE)
  expect_equal(newDF$OM[newDF$parameter_names == "OMEGA(1,1)"], TRUE)
  expect_equal(newDF$S[newDF$parameter_names == "SIGMA(1,1)"], TRUE)
  expect_equal(newDF$S[newDF$parameter_names == "THETA1"], FALSE)
})

test_that("define_param_table expected output:  generates logical columns for transformation [MPT-DPT-001]", {
  expect_true(newDF$trans[newDF$name == "THETA1"] == "logTrans" &
                newDF$LOG[newDF$name == "THETA1"] == TRUE &
                newDF$LOGIT[newDF$name == "THETA1"] == FALSE)

  expect_true(newDF$trans[newDF$name == "OMEGA11"] == "lognormalOm" &
                newDF$LOG[newDF$name == "OMEGA11"] == FALSE &
                newDF$lognormO[newDF$name == "OMEGA11"] == TRUE)

  expect_true(newDF$trans[newDF$name == "SIGMA11"] == "propErr" &
                newDF$LOG[newDF$name == "SIGMA11"] == FALSE &
                newDF$propErr[newDF$name == "SIGMA11"] == TRUE)
})

test_that("define_param_table incorrect estimate input type: no parameter_names column [MPT-DPT-002]",{
  param_est2 <- param_est
  colnames(param_est2)[colnames(param_est2) == "parameter_names"] ="no_name"
  expect_error(capture.output(define_param_table(param_est2, paramKey)))
})

test_that("define_param_table incorrect paramter key input type: missing column(s) [MPT-DPT-002]",{
  paramKey2 <- as.data.frame(paramKey)
  colnames(paramKey2)[colnames(paramKey2) == "panel"] ="no_name"
  expect_error(capture.output(define_param_table(param_est, paramKey2)))
})

