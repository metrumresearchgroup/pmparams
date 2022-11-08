pathDF <- define_param_table(param_est, paramKey)

test_that("getpRSE accurately calculates % RSE ", {
  param_RSE <- getpRSE(.df = pathDF, .value = estimate, .se = stderr)
  expect_equal(param_RSE$pRSE[param_RSE$name == "OMEGA11"], "24.0")
  expect_equal(param_RSE$pRSE[param_RSE$name == "OMEGA22"], "11.9")
  expect_equal(param_RSE$pRSE[param_RSE$name == "THETA4"], "1.93")
  expect_equal(unique(param_RSE$pRSE[param_RSE$LOG == "FALSE" & param_RSE$LOGIT == "FALSE" & param_RSE$TH == "FALSE" & param_RSE$diag == "FALSE"]), "-")
})

test_that("getpRSE maxex options for % RSE ", {
  pathDF$stderr[1] <- 19
  param_RSE <- getpRSE(.df = pathDF, .value = estimate, .se = stderr, .digit = 3, maxex = 4)
  expect_equal(param_RSE$pRSE[1], "2.46e+80")
})

test_that("getpRSE  digits options for % RSE ", {
  param_RSE <- getpRSE(.df = pathDF, .value = estimate, .se = stderr, .digit = 3)
  expect_equal(nchar(param_RSE$pRSE[param_RSE$name == "OMEGA11"]), 4)
})
