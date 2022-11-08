#boot_quantiles
x <- 1:10

test_that("boot quantile works with user selecting prob [MPT-BQU-001]", {
  prob = c(0.67)
  expect_equal(qt(x, prob), 7.03)
})

test_that("boot quantile works with high, median, and low prob [MPT-BQU-002]", {
  expect_equal(med(x), 5.5)
  expect_equal(med(x), qt(x, 0.5))
})

test_that("boot quantile low", {
  expect_equal(lo(x), 1.225)
  expect_equal(lo(x), qt(x, 0.025))
})

test_that("boot quantile high", {
  expect_equal(hi(x), 9.775)
  expect_equal(hi(x), qt(x, 0.975))
})
