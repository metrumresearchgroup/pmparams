Theoph_ex <- Theoph %>% dplyr::mutate(stderr = conc/10)
r_val <- function(val) round(val, 5)


test_that("getCI accurately calculates the 95% confidence interval", {
  Theoph_CI <- getCI(.df = Theoph_ex, .value = "conc", .se = "stderr", .ci = 95)
  expect_true(Theoph_CI$lower[1] < Theoph_CI$conc[1])
  expect_true(Theoph_CI$upper[108] > Theoph_CI$conc[108])
  expect_equal(r_val(Theoph_CI$upper[15]), 9.93873)
  expect_equal(r_val(Theoph_CI$lower[47]), 4.52654)
  expect_equal(Theoph_CI$conc[50] - Theoph_CI$lower[50], Theoph_CI$upper[50] - Theoph_CI$conc[50])
})


test_that("getCI accurately calculates the 90% confidence interval", {
  Theoph_CI <- getCI(.df = Theoph_ex, .value = "conc", .se = "stderr", .ci = 90)
  expect_true(Theoph_CI$lower[1] < Theoph_CI$conc[1])
  expect_true(Theoph_CI$upper[108] > Theoph_CI$conc[108])
  expect_equal(r_val(Theoph_CI$upper[15]), 9.67687)
  expect_equal(r_val(Theoph_CI$lower[47]), 4.70395)
  expect_equal(Theoph_CI$conc[50] - Theoph_CI$lower[50], Theoph_CI$upper[50] - Theoph_CI$conc[50])
})

test_that("getCI accurately calculates the 80% confidence interval", {
  Theoph_CI <- getCI(.df = Theoph_ex, .value = "conc", .se = "stderr", .ci = 80)
  expect_true(Theoph_CI$lower[1] < Theoph_CI$conc[1])
  expect_true(Theoph_CI$upper[108] > Theoph_CI$conc[108])
  expect_equal(r_val(Theoph_CI$upper[15]), 9.37497)
  expect_equal(r_val(Theoph_CI$lower[47]), 4.90849)
  expect_equal(Theoph_CI$conc[50] - Theoph_CI$lower[50], Theoph_CI$upper[50] - Theoph_CI$conc[50])
})
