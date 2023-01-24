
test_that("defineParamTable expected output: creates new parameter names without parentheses [MPT-DPT-001]", {
  expect_equal(newDF$name[newDF$parameter_names == "OMEGA(1,1)"], "OMEGA11")
})

test_that("defineParamTable expected output: generates logical columns to indicate parameter type [MPT-DPT-001]", {
  expect_equal(newDF$TH[newDF$parameter_names == "THETA1"], TRUE)
  expect_equal(newDF$OM[newDF$parameter_names == "THETA1"], FALSE)
  expect_equal(newDF$OM[newDF$parameter_names == "OMEGA(1,1)"], TRUE)
  expect_equal(newDF$S[newDF$parameter_names == "SIGMA(1,1)"], TRUE)
  expect_equal(newDF$S[newDF$parameter_names == "THETA1"], FALSE)
})

test_that("defineParamTable expected output:  generates logical columns for transformation [MPT-DPT-001]", {
  expect_true(newDF$trans[newDF$name == "THETA1"] == "logTrans" &
                newDF$LOG[newDF$name == "THETA1"] == TRUE &
                newDF$LOGIT[newDF$name == "THETA1"] == FALSE)

  expect_true(newDF$trans[newDF$name == "OMEGA11"] == "lognormalOm" &
                newDF$LOG[newDF$name == "OMEGA11"] == FALSE &
                newDF$lognormO[newDF$name == "OMEGA11"] == TRUE)

  expect_true(newDF$trans[newDF$name == "SIGMA11"] == "propErr" &
                newDF$LOG[newDF$name == "SIGMA11"] == FALSE &
                newDF$propErr[newDF$name == "SIGMA11"] == TRUE)
})

test_that("defineParamTable incorrect input type: no parameter_names column [MPT-DPT-002]",{
  param_est2 <- param_est
  colnames(param_est2)[colnames(param_est2) == "parameter_names"] ="no_name"
  expect_error(capture.output(defineParamTable(param_est2, paramKey)))
})

test_that("defineParamTable incorrect input type: missing column(s) [MPT-DPT-002]",{
  paramKey2 <- as.data.frame(paramKey)
  colnames(paramKey2)[colnames(paramKey2) == "panel"] ="no_name"
  expect_error(capture.output(defineParamTable(param_est, paramKey2)))
})

test_that("defineParamTable handles multiple estimate input types [MPT-DPT-003]", {
  pathDF <- defineParamTable(param_path, paramKey)
  expect_equal(pathDF$estimate[pathDF$name == "OMEGA22"], 0.0826922)

  mod_est <- bbr::read_model(system.file("model/nonmem/102", package = "mrgparamtab"))
  pathDF2 <- defineParamTable(mod_est, paramKey)
  expect_equal(pathDF2$estimate[pathDF2$name == "OMEGA22"], 0.0826922)

  pathDF3 <- defineParamTable(param_model, paramKey)
  expect_equal(pathDF3$estimate[pathDF3$name == "OMEGA22"], 0.0826922)

  pathDF4 <- defineParamTable(param_est, paramKey)
  expect_equal(pathDF4$estimate[pathDF4$name == "OMEGA22"], 0.0826922)

})

test_that("defineParamTable handles multiple parameter key input types [MPT-DPT-004]", {
  pathDF <- defineParamTable(param_path, system.file("model/nonmem/pk-parameter-key-new.yaml", package = "mrgparamtab"))
  expect_equal(pathDF$estimate[pathDF$name == "OMEGA22"], 0.0826922)
})

test_that("defineParamTable handles multiple parameter key input types [MPT-DPT-004]", {
  key_file <- system.file("model/nonmem/pk-parameter-key.yaml", package = "mrgparamtab")
  key_df <- pmtables::yaml_as_df(key_file)
  pathDF <- defineParamTable(param_path, key_df)
  expect_equal(pathDF$estimate[pathDF$name == "OMEGA22"], 0.0826922)
})

test_that("defineParamTable incorrect parameter key input type: Only abb, desc, panel and trans arguments will be used, all others ignored [MPT-DPT-005]", {
  expect_warning(capture.output(defineParamTable(param_path, system.file("model/nonmem/pk-parameter-key-both.yaml", package = "mrgparamtab"))))
})
