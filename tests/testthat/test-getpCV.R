test_that("getpCV accurately calculates CV%", {
  pCV_DF <- getpCV(newDF) %>% select(cv)
  expect_equal(pCV_DF$cv[12], "20.0")
  expect_equal(pCV_DF$cv[4], "-")
  expect_equal(pCV_DF$cv[6], "49.7")
})
