
test_that("define_param_table creates new parameter names without parentheses", {
  expect_equal(newDF$name[newDF$parameter_names == "OMEGA(1,1)"], "OMEGA11")
})

test_that("define_param_table generates logical columns to indicate parameter type", {
  expect_equal(newDF$TH[newDF$parameter_names == "THETA1"], TRUE)
  expect_equal(newDF$OM[newDF$parameter_names == "THETA1"], FALSE)
  expect_equal(newDF$OM[newDF$parameter_names == "OMEGA(1,1)"], TRUE)
  expect_equal(newDF$S[newDF$parameter_names == "SIGMA(1,1)"], TRUE)
  expect_equal(newDF$S[newDF$parameter_names == "THETA1"], FALSE)
})

test_that("define_param_table accurately generates logical columns for transformation", {
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
