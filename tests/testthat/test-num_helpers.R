test_that("asNum converts variety of input types to a numeric [MPT-ANM-001]", {
  expect_equal(asNum(13), 13)
  expect_equal(asNum('13'), 13)
  expect_equal(asNum(as.factor(12)), 12)
})
