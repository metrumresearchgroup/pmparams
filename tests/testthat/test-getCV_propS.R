test_that("getCV_propS accurately calculates proportion CV [MPT-PCV-001]", {
  expect_equal(getCV_propS(4), 200)
  expect_equal(getCV_propS(16), 400)
})
