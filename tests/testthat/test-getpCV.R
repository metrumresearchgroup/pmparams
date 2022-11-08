pathDF <- define_param_table(param_est, paramKey)

test_that("getpCV accurately calculates CV%", {
  param_CV <- getpCV(.df = pathDF, .column = estimate)
  expect_equal(param_CV$cv[param_CV$name == "OMEGA11"], "49.7")
  expect_equal(param_CV$cv[param_CV$name == "OMEGA22"], "29.4")
  expect_equal(param_CV$cv[param_CV$name == "SIGMA11"], "20.0")
})
