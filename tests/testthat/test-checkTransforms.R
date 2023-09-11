#check transforms

test_that("checkTranforms unexpected input: no trans column", {
  expect_error(checkTransforms(newDF %>% select(-trans)))
})

test_that("checkTransforms tildes input: works with trans column with no tildes", {
  expect_equal(newDF %>% dplyr::mutate(transTHETA = NA), checkTransforms(newDF))
})

test_that("checkTransforms tildes input: works with trans column with tilde", {
  newDF2 <- newDF
  newDF2$trans[1] <- "logTrans~lognormalOm"
  newDF3 <- checkTransforms(newDF2)
  expect_equal(newDF3$trans[1], "logTrans")
})


