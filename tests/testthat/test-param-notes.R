

test_that("param_notes expected output: alpha",{
  eq2 <- param_notes(.ci = 95)
  alpha <- 0.05
  expect_equal(
    eq2$ciEq,
    paste0("CI = estimate $\\pm$ $z_{\\alpha/2}$ $\\cdot$ SE, $\\alpha = ", alpha, "$")
  )

  alpha <- 0.5
  eq3 <- param_notes(.ci = 50)
  expect_equal(
    eq3$ciEq,
    paste0("CI = estimate $\\pm$ $z_{\\alpha/2}$ $\\cdot$ SE, $\\alpha = ", alpha, "$")
  )
})

test_that("param_notes expected output: footnotes",{
  eq4 <- param_notes()
  expect_equal(11, length(eq4))
  expect_equal(anyNA(eq4), FALSE)
  expect_equal(eq4$cvOmegaEq,"CV\\% of log-normal omega = sqrt(exp(estimate) - 1) $\\cdot$ 100")
})
