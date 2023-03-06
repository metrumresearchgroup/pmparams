test_that("getCV_lognormO accurately calculates CV log normal [MPT-CVL-001]", {
  expect_equal(getCV_lognormO(16), 298095.78)
  expect_equal(getCV_lognormO(1), 131.08325)
})
