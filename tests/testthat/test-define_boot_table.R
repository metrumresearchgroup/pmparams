withr::with_options(list(bbr.bbi_exe_path = bbr::read_bbi_path()), {

  test_that("define_boot_table expected output: creates new parameter names without parentheses", {
    expect_equal(newbootDF$abb[newbootDF$desc == "Apparent peripheral volume"], "V3/F (L)")
  })

  test_that("define_boot_table expected output: generates logical columns to indicate parameter type", {
    expect_equal(newbootDF$TH[newbootDF$parameter_names == "THETA1"], TRUE)
    expect_equal(newbootDF$OM[newbootDF$parameter_names == "THETA1"], FALSE)
    expect_equal(newbootDF$OM[newbootDF$parameter_names == "OMEGA(1,1)"], TRUE)
    expect_equal(newbootDF$S[newbootDF$parameter_names == "SIGMA(1,1)"], TRUE)
    expect_equal(newbootDF$S[newbootDF$parameter_names == "THETA1"], FALSE)
  })


  test_that("define_boot_table expected output:  generates logical columns for transformation", {
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


  test_that("define_boot_table incorrect input type: missing column(s)",{
    paramKey2 <- paramKey
    names(paramKey2)[names(paramKey2) == "panel"] ="no_name"
    expect_error(
      define_boot_table(boot_paramEstPath, paramKey2),
      "The following required columns are missing: panel"
    )
  })

  test_that("define_boot_table handles multiple estimate input types", {
    # Test file path
    boot_df1 <- define_boot_table(boot_paramEstPath, .key = paramKey)
    expect_equal(boot_df1$perc_50[boot_df1$name == "OMEGA22"], 0.0821058)

    # Test read-in dataframe
    boot_df2 <- define_boot_table(boot_paramEst, .key = paramKey)
    expect_equal(boot_df2$perc_50[boot_df2$name == "OMEGA22"], 0.0821058)

    # Test with bbr
    skip_if_missing_deps("bbr", "1.11.0")
    boot_df3 <- define_boot_table(
      bbr::bootstrap_estimates(BOOT_RUN), .key = paramKey
    )
    expect_equal(boot_df3$perc_50[boot_df3$name == "OMEGA22"], 0.08158385)
  })

  test_that("define_boot_table handles multiple parameter key input types", {
    # Key is a file path
    boot_df <- define_boot_table(boot_paramEst, .key = paramKey_path)
    expect_equal(boot_df$perc_50[boot_df$name == "OMEGA22"],  0.0821058)

    # Key is a data frame (row_var must be 'name')
    key <- pmtables::yaml_as_df(paramKey_path, row_var = "name")
    boot_df <- define_boot_table(boot_paramEst, .key = key)
    expect_equal(boot_df$perc_50[boot_df$name == "OMEGA22"], 0.0821058)
  })


  test_that("define_boot_table incorrect parameter key input type", {
    # TODO: There should be a loadParamKey test file that tests this, rather
    # than having it here and in define_param_table
    expect_warning(
      define_boot_table(boot_paramEst, paramKeyBoth_path),
      "Only abb, desc, panel and trans arguments will be used"
    )
  })

  test_that("define_boot_table generates the confidence intervals for various inputs", {
    boot_df <- define_boot_table(boot_paramEst, .key = paramKey)
    expect_equal(boot_df$perc_2.5[1], 1.3880675)
    expect_equal(boot_df$perc_97.5[2], 65.053174)
  })


  test_that("define_boot_table works with iqr", {
    boot_df <- define_boot_table(
      boot_paramEst, .key = paramKey, .ci = "iqr"
    )
    # This also confirms the correct percent names were chosen
    expect_equal(boot_df$perc_25[1], 1.50140037)
    expect_equal(boot_df$perc_75[2], 62.717938)
  })

  test_that("define_boot_table: if ci and percentile supplied, choose percentiles", {
    # Note that .ci has a default (95), and .percentiles defaults to NULL
    boot_df <- define_boot_table(
      boot_paramEst, .key = paramKey, .ci = "iqr", .percentiles = c(0.2, 0.6)
    )
    expect_equal(boot_df$perc_20[1], 1.4835063)
    expect_equal(boot_df$perc_60[2], 61.966381)
  })

  test_that("define_boot_table: 3 percentiles get renamed lower, value, upper", {
    boot_df <- define_boot_table(
      boot_paramEst, .key = paramKey, .percentiles = c(0.2, 0.6, 0.11)
    )
    perc_names <- boot_df %>% select(starts_with("perc_")) %>% names()
    expect_equal(perc_names, c("perc_11", "perc_20", "perc_60"))
  })


  test_that("define_boot_table error message if .percentile is not numeric list", {
    expect_error(
      define_boot_table(boot_paramEst, .key = paramKey, .percentiles = c("0.26", "0.55")),
      "Must be of type 'numeric'"
    )
  })

  #TODO:
  #add more percentile checks
})


