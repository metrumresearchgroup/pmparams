#check transforms

test_that("checkTranforms unexpected input: no trans column [MPT-CTF-001]", {
  expect_error(checkTransforms(newDF %>% select(-trans)))
})

test_that("checkTransforms works with trans column with no tildes [MPT-CTF-002]", {
  expect_equal(newDF %>% dplyr::mutate(transTHETA = NA), checkTransforms(newDF))
})

test_that("checkTransforms works with trans column with tilde [MPT-CTF-002]", {
  newDF2 <- newDF
  newDF2$trans[1] <- "logTrans~lognormalOm"
  newDF3 <- checkTransforms(newDF2)
  expect_equal(newDF3$trans[1], "logTrans")
})


