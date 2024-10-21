#param notes

test_that("param_notes unexpected input: confidence internal that is not 90 or 95 without accompanying z-score",{
  expect_error(
    param_notes(.ci = 75),
    regexp = "Z-score \\(.zscore\\) must be supplied when CI is not 90 or 95"
  )
})

test_that("param_notes unexpected input: confidence internal and z-score mismatch",{
  expect_message(
    param_notes(.ci = 95, .zscore = 1.9),
    regexp = "Confidence interval and z-score provided do not match. The z-score that corresponds to 95% CI will be used \\(z-score = 1\\.96\\)"

  )
})

test_that("param_notes expected output: z-score",{
  eq2 <- param_notes(.ci = 95)
  expect_equal(eq2$ciEq, paste0("CI = estimate $\\pm$ ", 1.96, " $\\cdot$ SE"))

  z = 1.4
  eq3 <- param_notes(.ci = 5, .zscore = z)
  expect_equal(eq3$ciEq, paste0("CI = estimate $\\pm$ ", z, " $\\cdot$ SE"))

})

test_that("param_notes expected output: footnotes",{
  eq4 <- param_notes()
  expect_equal(12, length(eq4))
  expect_equal(anyNA(eq4), FALSE)
  expect_equal(eq4$cvOmegaEq,"CV\\% of log-normal omegas = sqrt(exp(estimate) - 1) $\\cdot$ 100")


})
