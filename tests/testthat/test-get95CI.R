Theoph_ex <- Theoph %>% dplyr::mutate(stderr = conc/10)

test_that("get95CI accurately calculates the 95% confidence interval [MPT-CIF-001]", {
  Theoph_CI <- get95CI(.df = Theoph_ex, .value = "conc", .se = "stderr")
  expect_true(Theoph_CI$lower[1] < Theoph_CI$conc[1])
  expect_true(Theoph_CI$upper[108] > Theoph_CI$conc[108])
  expect_equal(Theoph_CI$upper[15], 9.93876)
  expect_equal(Theoph_CI$lower[47], 4.52652)
  expect_equal(Theoph_CI$conc[50] - Theoph_CI$lower[50], Theoph_CI$upper[50] - Theoph_CI$conc[50])
})
