#check transforms

test_that("checkTranforms unexpected input: no trans column", {
  expect_error(checkTransforms(PARAM_TAB_102 %>% select(-trans)))
})

test_that("checkTransforms tildes input: works with trans column with no tildes", {
  expect_equal(PARAM_TAB_102 %>% dplyr::mutate(transTHETA = NA), checkTransforms(PARAM_TAB_102))
})

test_that("checkTransforms tildes input: works with trans column with tilde", {
  newDF2 <- PARAM_TAB_102
  newDF2$trans[1] <- "logTrans~lognormalOm"
  newDF3 <- checkTransforms(newDF2)
  expect_equal(newDF3$trans[1], "logTrans")
})


