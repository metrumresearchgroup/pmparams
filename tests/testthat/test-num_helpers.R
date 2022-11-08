test_that("asNum converts variety of input types to a numeric [MPT-ANM-001]", {
  expect_equal(asNum(13), 13)
  expect_equal(asNum('13'), 13)
  expect_equal(asNum(as.factor(12)), 12)
})

test_that("getEvenNo returns number if value is even, empty if value is not even [MPT-ANM-002]", {
  expect_equal(getEvenNo(14), 14)
  expect_equal(getEvenNo(13), numeric(0))
})

