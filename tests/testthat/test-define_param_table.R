withr::with_options(list(bbr.bbi_exe_path = bbr::read_bbi_path()), {

  test_that("define_param_table expected output: creates new parameter names without parentheses", {
    expect_equal(newDF$name[newDF$parameter_names == "OMEGA(1,1)"], "OMEGA11")
  })

  test_that("define_param_table expected output: generates logical columns to indicate parameter type", {
    expect_equal(newDF$TH[newDF$parameter_names == "THETA1"], TRUE)
    expect_equal(newDF$OM[newDF$parameter_names == "THETA1"], FALSE)
    expect_equal(newDF$OM[newDF$parameter_names == "OMEGA(1,1)"], TRUE)
    expect_equal(newDF$S[newDF$parameter_names == "SIGMA(1,1)"], TRUE)
    expect_equal(newDF$S[newDF$parameter_names == "THETA1"], FALSE)
  })

  test_that("define_param_table expected output:  generates logical columns for transformation", {
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

  test_that("define_param_table incorrect input type: no parameter_names column",{
    paramEst2 <- paramEst
    colnames(paramEst2)[colnames(paramEst2) == "parameter_names"] ="no_name"
    expect_error(
      define_param_table(paramEst2, paramKey),
      "`parameter_names` is an expected column name"
    )
  })

  test_that("define_param_table incorrect input type: missing column(s)",{
    paramKey2 <- as.data.frame(paramKey)
    colnames(paramKey2)[colnames(paramKey2) == "panel"] ="no_name"
    expect_error(
      define_param_table(paramEst, paramKey2),
      "The following required columns are missing: panel"
    )
  })

  test_that("define_param_table handles multiple estimate input types", {
    skip_if_missing_deps("bbr")

    # Path to bbr model
    mod_path <- file.path(model_dir, "102")
    pathDF2 <- define_param_table(mod_path, paramKey)
    expect_equal(pathDF2$estimate[pathDF2$name == "OMEGA22"], 0.0826922)

    # bbr model
    mod <- bbr::read_model(mod_path)
    pathDF3 <- define_param_table(mod, paramKey)
    expect_equal(pathDF3$estimate[pathDF3$name == "OMEGA22"], 0.0826922)

    # bbr model summary
    mod_sum <- bbr::model_summary(mod)
    pathDF4 <- define_param_table(mod_sum, paramKey)
    expect_equal(pathDF4$estimate[pathDF4$name == "OMEGA22"], 0.0826922)

    # bbr parameter estimates
    mod_est <- bbr::param_estimates(mod_sum)
    pathDF4 <- define_param_table(mod_est, paramKey)
    expect_equal(pathDF4$estimate[pathDF4$name == "OMEGA22"], 0.0826922)
  })

  test_that("define_param_table handles multiple parameter key input types", {
    param_df <- define_param_table(paramPath, paramKey_path)
    expect_equal(param_df$estimate[param_df$name == "OMEGA22"], 0.0826922)

    key_file <- file.path(model_dir, "pk-parameter-key.yaml")
    key_df <- pmtables::yaml_as_df(key_file)
    param_df <- define_param_table(paramPath, key_df)
    expect_equal(param_df$estimate[param_df$name == "OMEGA22"], 0.0826922)
  })

  test_that("define_param_table incorrect parameter key input type", {
    # TODO: There should be a loadParamKey test file that tests this, rather
    # than having it here and in define_boot_table
    expect_warning(
      define_param_table(paramPath, paramKeyBoth_path),
      "Only abb, desc, panel and trans arguments will be used"
    )
  })

  test_that("define_param_table generates correct corr_SD", {
    expect_true(all(newDF$estimate == newDF$value))
    expect_true(all(newDF$stderr == newDF$se))
    expect_true(newDF$corr_SD[9] == "0.510933")
    expect_true(is.na(newDF$corr_SD[1]))
    expect_true(is.na(newDF$corr_SD[6]))
  })

  test_that("define_param_table generates the confidence intervals for various inputs", {
    newDF_ci95 <- define_param_table(.estimates = paramEst, .key = paramKey, .ci = 95, .zscore = NULL)
    newDF_ci90 <- define_param_table(.estimates = paramEst, .key = paramKey, .ci = 90, .zscore = NULL)

    expect_equal(newDF_ci90$lower[1], 0.33047798)
    expect_equal(newDF_ci90$upper[2], 4.1640688)

    expect_equal(newDF_ci95$lower[4], 4.1721829)
    expect_equal(newDF_ci95$upper[7], 0.10195012)
  })

  test_that("define_param_table expected dataframe: respects yaml key order",{
    paramKey2 <- paramKey %>%
      filter(name %in% newDF$name)

    expect_equal(newDF$name, paramKey2$name)
  })

  test_that("define_param_table message if using theta error block", {
    expect_message(define_param_table(.estimates = theta_err, .key = theta_err_key, .ci = 95, .zscore = NULL))
  })


})
