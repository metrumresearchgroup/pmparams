
test_that("defineBootTable expected output: creates new parameter names without parentheses [MPT-DBT-001]", {
  expect_equal(newbootDF$abb[newbootDF$desc == "Apparent peripheral volume"], "V3/F (L)")
})

test_that("defineBootTable expected output: generates logical columns to indicate parameter type [MPT-DBT-001]", {
  expect_equal(newbootDF$TH[newbootDF$parameter_names == "THETA1"], TRUE)
  expect_equal(newbootDF$OM[newbootDF$parameter_names == "THETA1"], FALSE)
  expect_equal(newbootDF$OM[newbootDF$parameter_names == "OMEGA.1.1."], TRUE)
  expect_equal(newbootDF$S[newbootDF$parameter_names == "SIGMA.1.1."], TRUE)
  expect_equal(newbootDF$S[newbootDF$parameter_names == "THETA1"], FALSE)
})

# test_that("defineBootTable expected output:  generates logical columns for transformation [MPT-DBT-001]", {
#   expect_true(newbootDF$trans[newbootDF$name == "THETA1"] == "logTrans" &
#                 newbootDF$LOG[newbootDF$name == "THETA1"] == TRUE &
#                 newbootDF$LOGIT[newbootDF$name == "THETA1"] == FALSE)
#
#   expect_true(newbootDF$trans[newbootDF$name == "OMEGA11"] == "lognormalOm" &
#                 newbootDF$LOG[newbootDF$name == "OMEGA11"] == FALSE &
#                 newbootDF$lognormO[newbootDF$name == "OMEGA11"] == TRUE)
#
#   expect_true(newbootDF$trans[newbootDF$name == "SIGMA11"] == "propErr" &
#                 newbootDF$LOG[newbootDF$name == "SIGMA11"] == FALSE &
#                 newbootDF$propErr[newbootDF$name == "SIGMA11"] == TRUE)
# })
#
# # test_that("defineBootTable incorrect input type: no parameter_names column [MPT-DBT-002]",{
# #   boot_param_est2 <- boot_param_est
# #   colnames(boot_param_est2)[colnames(boot_param_est2) == "run"] ="no_name"
# #   expect_error(capture.output(defineBootTable(boot_param_est2, paramKey)))
# # })
#
# test_that("defineBootTable incorrect input type: missing column(s) [MPT-DBT-002]",{
#   paramKey2 <- as.data.frame(paramKey)
#   colnames(paramKey2)[colnames(paramKey2) == "panel"] ="no_name"
#   expect_error(capture.output(defineBootTable(param_est, paramKey2)))
# })
#
# test_that("defineBootTable handles multiple estimate input types [MPT-DBT-003]", {
#   skip_if_no_bbi("MPT-DPT-003")
#   pathnewbootDF <- defineBootTable(param_bootpath, paramKey)
#   expect_equal(pathDF$estimate[pathDF$name == "OMEGA22"], 0.0826922)
#
#   mod_est <- bbr::read_model(system.file("model/nonmem/102", package = "mrgparamtab"))
#   pathDF2 <- defineBootTable(mod_est, paramKey)
#   expect_equal(pathDF2$estimate[pathDF2$name == "OMEGA22"], 0.0826922)
#
#   pathDF3 <- defineBootTable(param_model, paramKey)
#   expect_equal(pathDF3$estimate[pathDF3$name == "OMEGA22"], 0.0826922)
#
#   pathDF4 <- defineBootTable(param_model %>% bbr::model_summary(), paramKey)
#   expect_equal(pathDF4$estimate[pathDF4$name == "OMEGA22"], 0.0826922)
#
# })
#
# test_that("defineBootTable handles multiple parameter key input types [MPT-DBT-004]", {
#   skip_if_no_bbi("MPT-DPT-004")
#   pathDF <- defineBootTable(param_path, system.file("model/nonmem/pk-parameter-key-new.yaml", package = "mrgparamtab"))
#   expect_equal(pathDF$estimate[pathDF$name == "OMEGA22"], 0.0826922)
# })
#
# test_that("defineBootTable handles multiple parameter key input types [MPT-DBT-004]", {
#   skip_if_no_bbi("MPT-DPT-004")
#   key_file <- system.file("model/nonmem/pk-parameter-key.yaml", package = "mrgparamtab")
#   key_df <- pmtables::yaml_as_df(key_file)
#   pathDF <- defineBootTable(param_path, key_df)
#   expect_equal(pathDF$estimate[pathDF$name == "OMEGA22"], 0.0826922)
# })
#
# test_that("defineBootTable incorrect parameter key input type: Only abb, desc, panel and trans arguments will be used, all others ignored [MPT-DBT-005]", {
#   skip_if_no_bbi("MPT-DPT-005")
#   expect_warning(capture.output(defineBootTable(param_path, system.file("model/nonmem/pk-parameter-key-both.yaml", package = "mrgparamtab"))))
# })
#
# test_that("defineBootTable generates correct corr_SD [MPT-DBT-005]", {
#   expect_true(all(newDF$estimate == newDF$value))
#   expect_true(all(newDF$stderr == newDF$se))
#   expect_true(newDF$corr_SD[7] == "0.511")
#   expect_true(newDF$corr_SD[1] == "-")
#   expect_true(newDF$corr_SD[6] == "-")
# })
#
# test_that("defineBootTable generates the confidence intervals for various inputs [MPT-DBT-006]", {
#   newDF_ci95 <- defineBootTable(.estimates = param_est, .key = paramKey, .ci = 95, .zed = NULL)
#   newDF_ci90 <- defineBootTable(.estimates = param_est, .key = paramKey, .ci = 90, .zed = NULL)
#
#   expect_equal(newDF_ci90$lower[1], 0.33047798)
#   expect_equal(newDF_ci90$upper[2], 4.1640688)
#
#   expect_equal(newDF_ci95$lower[4], 4.1721829)
#   expect_equal(newDF_ci95$upper[7], 0.108133732)
# })

