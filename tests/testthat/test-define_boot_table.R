
test_that("define_boot_table expected output: creates new parameter names without parentheses [MPT-DBT-001]", {
  expect_equal(newbootDF$abb[newbootDF$desc == "Apparent peripheral volume"], "V3/F (L)")
})

test_that("define_boot_table expected output: generates logical columns to indicate parameter type [MPT-DBT-001]", {
  expect_equal(newbootDF$TH[newbootDF$parameter_names == "THETA1"], TRUE)
  expect_equal(newbootDF$OM[newbootDF$parameter_names == "THETA1"], FALSE)
  expect_equal(newbootDF$OM[newbootDF$parameter_names == "OMEGA.1.1."], TRUE)
  expect_equal(newbootDF$S[newbootDF$parameter_names == "SIGMA.1.1."], TRUE)
  expect_equal(newbootDF$S[newbootDF$parameter_names == "THETA1"], FALSE)
})


test_that("define_boot_table expected output:  generates logical columns for transformation [MPT-DBT-001]", {
  expect_true(newbootDF$trans[newbootDF$name == "THETA1"] == "logTrans" &
                newbootDF$LOG[newbootDF$name == "THETA1"] == TRUE &
                newbootDF$LOGIT[newbootDF$name == "THETA1"] == FALSE)

  expect_true(newbootDF$trans[newbootDF$name == "OMEGA11"] == "lognormalOm" &
                newbootDF$LOG[newbootDF$name == "OMEGA11"] == FALSE &
                newbootDF$lognormO[newbootDF$name == "OMEGA11"] == TRUE)

  expect_true(newbootDF$trans[newbootDF$name == "SIGMA11"] == "propErr" &
                newbootDF$LOG[newbootDF$name == "SIGMA11"] == FALSE &
                newbootDF$propErr[newbootDF$name == "SIGMA11"] == TRUE)
})

test_that("define_boot_table incorrect input type: no parameter_names column [MPT-DBT-002]",{
  boot_paramEst2 <- boot_paramEst
  colnames(boot_paramEst2)[colnames(boot_paramEst2) == "run"] ="no_name"
  expect_error(capture.output(define_boot_table(boot_paramEst2, paramKey)))
})

test_that("define_boot_table incorrect input type: missing column(s) [MPT-DBT-002]",{
  paramKey2 <- as.data.frame(paramKey)
  colnames(paramKey2)[colnames(paramKey2) == "panel"] ="no_name"
  expect_error(capture.output(define_boot_table(paramEst, paramKey2)))
})

test_that("define_boot_table handles multiple estimate input types [MPT-DBT-003]", {
  skip_if_no_bbi("MPT-DPT-003")
  pathnewbootDF <-define_boot_table(.boot_estimates =boot_paramEst, .nonboot_estimates = nonboot_paramEst, .key = paramKey)
  expect_equal(pathnewbootDF$estimate[pathnewbootDF$name == "OMEGA22"], 0.0821058)

  mod_est <- bbr::read_model(system.file("model/nonmem/106", package = "pmparams"))
  pathDF2 <- define_boot_table(.boot_estimates = boot_paramEstPath, .nonboot_estimates = nonboot_paramEstPath, .key = paramKey)
  expect_equal(pathDF2$estimate[pathDF2$name == "OMEGA22"], 0.0821058)

  pathDF3 <- define_boot_table(.boot_estimates =boot_paramEst, .nonboot_estimates = mod_est, .key = paramKey)
  expect_equal(pathDF3$estimate[pathDF3$name == "OMEGA22"], 0.0821058)

})

test_that("define_boot_table handles multiple parameter key input types [MPT-DBT-004]", {
  skip_if_no_bbi("MPT-DPT-004")
  pathDF <- define_boot_table(.boot_estimates =boot_paramEst,
                            .nonboot_estimates = nonboot_paramEst,
                            .key = system.file("model/nonmem/pk-parameter-key-new.yaml", package = "pmparams"))
  expect_equal(pathDF$estimate[pathDF$name == "OMEGA22"],  0.0821058)
})

test_that("define_boot_table handles multiple parameter key input types [MPT-DBT-004]", {
  skip_if_no_bbi("MPT-DPT-004")
  key_file <- system.file("model/nonmem/pk-parameter-key.yaml", package = "pmparams")
  key_df <- pmtables::yaml_as_df(key_file)
  pathDF <- define_boot_table(.boot_estimates =boot_paramEst,
                            .nonboot_estimates = nonboot_paramEst,
                            .key = key_df)
  expect_equal(pathDF$estimate[pathDF$name == "OMEGA22"], 0.0821058)
})

test_that("define_boot_table incorrect parameter key input type: Only abb, desc, panel and trans arguments will be used, all others ignored [MPT-DBT-005]", {
  skip_if_no_bbi("MPT-DPT-005")
  expect_warning(capture.output(define_boot_table(.boot_estimates =boot_paramEst,
                                                .nonboot_estimates = nonboot_paramEst,
                                                .key = system.file("model/nonmem/pk-parameter-key-both.yaml", package = "pmparams"))))
})

# #for boot, estimates do not equal values////
# test_that("define_boot_table generates correct corr_SD [MPT-DBT-005]", {
#   expect_true(all(newbootDF$estimate == newbootDF$value))
# })

test_that("define_boot_table generates the confidence intervals for various inputs [MPT-DBT-006]", {
  newbootDF <-define_boot_table(.boot_estimates =boot_paramEst, .nonboot_estimates = nonboot_paramEst, .key = paramKey)
  expect_equal(newbootDF$lower[1], 1.3880675)
  expect_equal(newbootDF$upper[2], 65.053174)
})


