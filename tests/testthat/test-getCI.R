Theoph_ex <- Theoph %>% dplyr::mutate(stderr = conc/10)
r_val <- function(val) round(val, 5)

test_ci <- function(lower_exp, upper_exp, .ci = 95, .df = Theoph_ex){
  Theoph_CI <- getCI(.df = .df, .value = "conc", .se = "stderr", .ci = .ci)
  expect_true(Theoph_CI$lower[1] < Theoph_CI$conc[1])
  expect_true(Theoph_CI$upper[108] > Theoph_CI$conc[108])
  expect_equal(r_val(Theoph_CI$upper[15]), upper_exp)
  expect_equal(r_val(Theoph_CI$lower[47]), lower_exp)
  expect_equal(Theoph_CI$conc[50] - Theoph_CI$lower[50], Theoph_CI$upper[50] - Theoph_CI$conc[50])
}

test_that("getCI accurately calculates a confidence interval", {
  expecations <- list(
    .ci = c(95, 90, 80, 50),
    lower_exp = c(4.52654, 4.70395, 4.90849, 5.25026),
    upper_exp = c(9.93873, 9.67687, 9.37497, 8.87050)
  )

  purrr::pwalk(expecations, test_ci)
})

