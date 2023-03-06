Theoph_ex <- Theoph %>% dplyr::mutate(stderr = conc/10)

test_that("getCI accurately calculates the 95% confidence interval [MPT-CIF-001]", {
  Theoph_CI <- getCI(.df = Theoph_ex, .value = "conc", .se = "stderr", .ci = 95)
  expect_true(Theoph_CI$lower[1] < Theoph_CI$conc[1])
  expect_true(Theoph_CI$upper[108] > Theoph_CI$conc[108])
  expect_equal(Theoph_CI$upper[15], 9.93876)
  expect_equal(Theoph_CI$lower[47], 4.52652)
  expect_equal(Theoph_CI$conc[50] - Theoph_CI$lower[50], Theoph_CI$upper[50] - Theoph_CI$conc[50])
})

test_that("getCI accurately calculates the 95% confidence interval using zed [MPT-CIF-001]", {
  Theoph_CI_a <- getCI(.df = Theoph_ex, .value = "conc", .se = "stderr", .ci = 95)
  Theoph_CI_b <- getCI(.df = Theoph_ex, .value = "conc", .se = "stderr", .ci = 95, .zed = 1.96)
  expect_equal(Theoph_CI_a, Theoph_CI_b)
})

test_that("getCI accurately calculates the 90% confidence interval [MPT-CIO-001]", {
  Theoph_CI <- getCI(.df = Theoph_ex, .value = "conc", .se = "stderr", .ci = 90)
  expect_true(Theoph_CI$lower[1] < Theoph_CI$conc[1])
  expect_true(Theoph_CI$upper[108] > Theoph_CI$conc[108])
  expect_equal(Theoph_CI$upper[15], 9.67284)
  expect_equal(Theoph_CI$lower[47], 4.70668)
  expect_equal(Theoph_CI$conc[50] - Theoph_CI$lower[50], Theoph_CI$upper[50] - Theoph_CI$conc[50])
})

test_that("getCI accurately calculates the 80% confidence interval [MPT-CIO-001]", {
  Theoph_CI <- getCI(.df = Theoph_ex, .value = "conc", .se = "stderr", .ci = 80, .zed = 1.282)
  expect_true(Theoph_CI$lower[1] < Theoph_CI$conc[1])
  expect_true(Theoph_CI$upper[108] > Theoph_CI$conc[108])
  expect_equal(Theoph_CI$upper[15], 9.375342)
  expect_equal(Theoph_CI$lower[47], 4.908234)
  expect_equal(Theoph_CI$conc[50] - Theoph_CI$lower[50], Theoph_CI$upper[50] - Theoph_CI$conc[50])
})
