withr::with_options(list(bbr.bbi_exe_path = bbr::read_bbi_path()), {

  test_that("define_boot_table expected output: creates new parameter names without parentheses", {
    expect_equal(BOOT_TAB_106$abb[BOOT_TAB_106$desc == "Apparent peripheral volume"], "V3/F (L)")
  })

  test_that("define_boot_table expected output: generates logical columns to indicate parameter type", {
    expect_equal(BOOT_TAB_106$TH[BOOT_TAB_106$parameter_names == "THETA1"], TRUE)
    expect_equal(BOOT_TAB_106$OM[BOOT_TAB_106$parameter_names == "THETA1"], FALSE)
    expect_equal(BOOT_TAB_106$OM[BOOT_TAB_106$parameter_names == "OMEGA(1,1)"], TRUE)
    expect_equal(BOOT_TAB_106$S[BOOT_TAB_106$parameter_names == "SIGMA(1,1)"], TRUE)
    expect_equal(BOOT_TAB_106$S[BOOT_TAB_106$parameter_names == "THETA1"], FALSE)
  })


  test_that("define_boot_table expected output:  generates logical columns for transformation", {
    expect_true(BOOT_TAB_106$trans[BOOT_TAB_106$name == "THETA1"] == "logTrans" &
                  BOOT_TAB_106$LOG[BOOT_TAB_106$name == "THETA1"] == TRUE &
                  BOOT_TAB_106$LOGIT[BOOT_TAB_106$name == "THETA1"] == FALSE)

    expect_true(BOOT_TAB_106$trans[BOOT_TAB_106$name == "OMEGA11"] == "lognormalOm" &
                  BOOT_TAB_106$LOG[BOOT_TAB_106$name == "OMEGA11"] == FALSE &
                  BOOT_TAB_106$lognormO[BOOT_TAB_106$name == "OMEGA11"] == TRUE)

    expect_true(BOOT_TAB_106$trans[BOOT_TAB_106$name == "SIGMA11"] == "propErr" &
                  BOOT_TAB_106$LOG[BOOT_TAB_106$name == "SIGMA11"] == FALSE &
                  BOOT_TAB_106$propErr[BOOT_TAB_106$name == "SIGMA11"] == TRUE)
  })

  test_that("define_boot_table incorrect input type: no parameter_names column",{
    boot_paramEst2 <- BOOT_106_EST
    colnames(boot_paramEst2)[colnames(boot_paramEst2) == "run"] ="no_name"
    expect_error(capture.output(define_boot_table(boot_paramEst2, PARAM_KEY_DF)))
  })

  test_that("define_boot_table incorrect input type: missing column(s)",{
    paramKey2 <- as.data.frame(PARAM_KEY_DF)
    colnames(paramKey2)[colnames(paramKey2) == "panel"] ="no_name"
    expect_error(capture.output(define_boot_table(PARAM_EST_102, paramKey2)))
  })

  test_that("define_boot_table handles multiple estimate input types", {
    pathnewbootDF <- define_boot_table(.boot_estimates = BOOT_106_EST, .nonboot_estimates = PARAM_EST_106, .key = PARAM_KEY_DF)
    expect_equal(BOOT_TAB_106$estimate[BOOT_TAB_106$name == "OMEGA22"], 0.0821058)

    pathDF2 <- define_boot_table(.boot_estimates = BOOT_106_EST_PATH, .nonboot_estimates = MOD106_PATH, .key = PARAM_KEY_DF)
    expect_equal(pathDF2$estimate[pathDF2$name == "OMEGA22"], 0.0821058)

    mod_est <- bbr::read_model(MOD106_PATH)
    pathDF3 <- define_boot_table(.boot_estimates = BOOT_106_EST, .nonboot_estimates = mod_est, .key = PARAM_KEY_DF)
    expect_equal(pathDF3$estimate[pathDF3$name == "OMEGA22"], 0.0821058)

  })

  test_that("define_boot_table handles multiple parameter key input types", {
    pathDF <- define_boot_table(.boot_estimates = BOOT_106_EST,
                                .nonboot_estimates = PARAM_EST_106,
                                .key = PARAM_KEY_PATH)
    expect_equal(pathDF$estimate[pathDF$name == "OMEGA22"],  0.0821058)
  })

  test_that("define_boot_table handles multiple parameter key input types", {
    key_df <- pmtables::yaml_as_df(PARAM_KEY_PATH_DF)
    pathDF <- define_boot_table(.boot_estimates = BOOT_106_EST,
                                .nonboot_estimates = PARAM_EST_106,
                                .key = key_df)
    expect_equal(pathDF$estimate[pathDF$name == "OMEGA22"], 0.0821058)
  })

  test_that("define_boot_table incorrect parameter key input type: Only abb, desc, panel and trans arguments will be used, all others ignored", {
    expect_warning(capture.output(define_boot_table(.boot_estimates = BOOT_106_EST,
                                                    .nonboot_estimates = PARAM_EST_106,
                                                    .key = PARAM_KEY_PATH_BOTH)))
  })

  # #for boot, estimates do not equal values////
  # test_that("define_boot_table generates correct corr_SD", {
  #   expect_true(all(newbootDF$estimate == newbootDF$value))
  # })

  test_that("define_boot_table generates the confidence intervals for various inputs", {
    expect_equal(BOOT_TAB_106$lower[1], 1.3880675)
    expect_equal(BOOT_TAB_106$upper[2], 65.053174)
  })

})
