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

  test_that("define_boot_table incorrect input type: missing column(s)",{
    paramKey2 <- as.data.frame(PARAM_KEY_DF)
    colnames(paramKey2)[colnames(paramKey2) == "panel"] ="no_name"
    expect_error(
      define_boot_table(BOOT_106_EST, .key = paramKey2),
      "The following required columns are missing: panel"
    )
  })

  test_that("define_boot_table handles multiple estimate input types", {
    # Test file path
    boot_df1 <- define_boot_table(BOOT_106_EST_PATH, .key = PARAM_KEY_DF)
    expect_equal(boot_df1$value[boot_df1$name == "OMEGA22"], 0.0821058)

    # Test read-in dataframe
    boot_df2 <- define_boot_table(BOOT_106_EST, .key = PARAM_KEY_DF)
    expect_equal(boot_df2$value[boot_df2$name == "OMEGA22"], 0.0821058)

    # Test with bbr
    skip_if_missing_deps("bbr", "1.11.0")
    boot_df3 <- define_boot_table(
      bbr::bootstrap_estimates(BOOT_RUN), .key = PARAM_KEY_DF
    )
    expect_equal(boot_df3$value[boot_df3$name == "OMEGA22"], 0.0790954)
  })

  test_that("define_boot_table handles multiple parameter key input types", {
    # Key is a file path
    boot_df <- define_boot_table(BOOT_106_EST, .key = PARAM_KEY_PATH)
    expect_equal(boot_df$value[boot_df$name == "OMEGA22"],  0.0821058)

    # Key is a data frame (row_var must be 'name')
    key <- pmtables::yaml_as_df(PARAM_KEY_PATH, row_var = "name")
    boot_df <- define_boot_table(BOOT_106_EST, .key = key)
    expect_equal(boot_df$value[boot_df$name == "OMEGA22"], 0.0821058)
  })

  test_that("define_boot_table incorrect parameter key input type", {
    # TODO: There should be a loadParamKey test file that tests this, rather
    # than having it here and in define_param_table
    expect_warning(
      define_boot_table(BOOT_106_EST, .key = PARAM_KEY_PATH_BOTH),
      "Only abb, desc, panel and trans arguments will be used"
    )
  })

  test_that("define_boot_table works with iqr", {
    boot_df <- define_boot_table(
      BOOT_106_EST, .key = PARAM_KEY_DF, .ci = "iqr"
    )
    # This also confirms the correct percent names were chosen
    expect_equal(boot_df$lower[1], 1.50140037)
    expect_equal(boot_df$upper[2], 62.717938)
  })

})
