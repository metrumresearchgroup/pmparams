#get equations

test_that("get_equations unexpected input: confidence internal that is not 90 or 95 without accompanying z-score",{
  expect_error(capture.output(get_equations(.ci = 75)))
})

test_that("get_equations unexpected input: confidence internal and z-score mismatch",{
  eq1 <- get_equations(.ci = 95, .zscore = 1.9)
  expect_equal(eq1$ciEq, paste0("Confidence intervals = estimate $\\pm$ ", 1.64, " $\\cdot$ SE"))
  expect_message(capture.output(get_equations(.ci = 95, .zscore = 1.9)))
})

test_that("get_equations expected output: z-score",{
  eq2 <- get_equations(.ci = 95)
  expect_equal(eq2$ciEq, paste0("Confidence intervals = estimate $\\pm$ ", 1.64, " $\\cdot$ SE"))

  z = 1.4
  eq3 <- get_equations(.ci = 5, .zscore = z)
  expect_equal(eq3$ciEq, paste0("Confidence intervals = estimate $\\pm$ ", z, " $\\cdot$ SE"))

})

test_that("get_equations expected output: footnotes",{
  eq4 <- get_equations()
  expect_equal(10, length(eq4))
  expect_equal(anyNA(eq4), FALSE)
  expect_equal(eq4$cvOmegaEq,"CV\\% of log-normal omegas = sqrt(exp(estimate) - 1) $\\cdot$ 100")


})
