Theoph_ex <- Theoph %>% dplyr::mutate(stderr = conc/10)

test_that("get90CI accurately calculates the 90% confidence interval [MPT-CIO-001]", {
  Theoph_CI <- get90CI(.df = Theoph_ex, .value = conc, .se = stderr)
  expect_true(Theoph_CI$lower[1] < Theoph_CI$conc[1])
  expect_true(Theoph_CI$upper[108] > Theoph_CI$conc[108])
  expect_equal(Theoph_CI$upper[15], 9.67284)
  expect_equal(Theoph_CI$lower[47], 4.70668)
  expect_equal(Theoph_CI$conc[50] - Theoph_CI$lower[50], Theoph_CI$upper[50] - Theoph_CI$conc[50])
})
