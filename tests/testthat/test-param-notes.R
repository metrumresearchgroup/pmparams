#get equations

test_that("param_notes unexpected input: confidence internal that is not 90 or 95 without accompanying z-score",{
  expect_error(capture.output(param_notes(.ci = 75)))
})

test_that("param_notes unexpected input: confidence internal and z-score mismatch",{
  eq1 <- param_notes(.ci = 95, .zscore = 1.9)
  expect_equal(eq1$ciEq, paste0("Confidence intervals = estimate $\\pm$ ", 1.96, " $\\cdot$ SE"))
  expect_message(capture.output(param_notes(.ci = 95, .zscore = 1.9)))
})

test_that("param_notes expected output: z-score",{
  eq2 <- param_notes(.ci = 95)
  expect_equal(eq2$ciEq, paste0("Confidence intervals = estimate $\\pm$ ", 1.96, " $\\cdot$ SE"))

  z = 1.4
  eq3 <- param_notes(.ci = 5, .zscore = z)
  expect_equal(eq3$ciEq, paste0("Confidence intervals = estimate $\\pm$ ", z, " $\\cdot$ SE"))

})

test_that("param_notes expected output: footnotes",{
  eq4 <- param_notes()
  expect_equal(10, length(eq4))
  expect_equal(anyNA(eq4), FALSE)
  expect_equal(eq4$cvOmegaEq,"CV\\% of log-normal omegas = sqrt(exp(estimate) - 1) $\\cdot$ 100")


})
