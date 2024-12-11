withr::with_options(list(bbr.bbi_exe_path = bbr::read_bbi_path()), {

  test_that("define_param_table expected output: creates new parameter names without parentheses", {
    expect_equal(PARAM_TAB_102$name[PARAM_TAB_102$parameter_names == "OMEGA(1,1)"], "OMEGA11")
  })

  test_that("define_param_table expected output: generates logical columns to indicate parameter type", {
    expect_equal(PARAM_TAB_102$TH[PARAM_TAB_102$parameter_names == "THETA1"], TRUE)
    expect_equal(PARAM_TAB_102$OM[PARAM_TAB_102$parameter_names == "THETA1"], FALSE)
    expect_equal(PARAM_TAB_102$OM[PARAM_TAB_102$parameter_names == "OMEGA(1,1)"], TRUE)
    expect_equal(PARAM_TAB_102$S[PARAM_TAB_102$parameter_names == "SIGMA(1,1)"], TRUE)
    expect_equal(PARAM_TAB_102$S[PARAM_TAB_102$parameter_names == "THETA1"], FALSE)
  })

  test_that("define_param_table expected output:  generates logical columns for transformation", {
    expect_true(PARAM_TAB_102$trans[PARAM_TAB_102$name == "THETA1"] == "logTrans" &
                  PARAM_TAB_102$LOG[PARAM_TAB_102$name == "THETA1"] == TRUE &
                  PARAM_TAB_102$LOGIT[PARAM_TAB_102$name == "THETA1"] == FALSE)

    expect_true(PARAM_TAB_102$trans[PARAM_TAB_102$name == "OMEGA11"] == "lognormalOm" &
                  PARAM_TAB_102$LOG[PARAM_TAB_102$name == "OMEGA11"] == FALSE &
                  PARAM_TAB_102$lognormO[PARAM_TAB_102$name == "OMEGA11"] == TRUE)

    expect_true(PARAM_TAB_102$trans[PARAM_TAB_102$name == "SIGMA11"] == "propErr" &
                  PARAM_TAB_102$LOG[PARAM_TAB_102$name == "SIGMA11"] == FALSE &
                  PARAM_TAB_102$propErr[PARAM_TAB_102$name == "SIGMA11"] == TRUE)
  })

  test_that("define_param_table incorrect input type: no parameter_names column",{
    paramEst2 <- PARAM_EST_102
    colnames(paramEst2)[colnames(paramEst2) == "parameter_names"] ="no_name"
    expect_error(
      define_param_table(paramEst2, PARAM_KEY_DF),
      "`parameter_names` is an expected column name"
    )
  })

  test_that("define_param_table incorrect input type: missing column(s)",{
    paramKey2 <- as.data.frame(PARAM_KEY_DF)
    colnames(paramKey2)[colnames(paramKey2) == "panel"] ="no_name"
    expect_error(
      define_param_table(PARAM_EST_102, paramKey2),
      "The following required columns are missing: panel"
    )
  })

  test_that("define_param_table handles multiple estimate input types", {
    skip_if_missing_deps("bbr")

    # Path to bbr model
    pathDF2 <- define_param_table(MOD102_PATH, PARAM_KEY_DF)
    expect_equal(pathDF2$estimate[pathDF2$name == "OMEGA22"], 0.0826922)

    # bbr model
    mod <- bbr::read_model(MOD102_PATH)
    pathDF3 <- define_param_table(mod, PARAM_KEY_DF)
    expect_equal(pathDF3$estimate[pathDF3$name == "OMEGA22"], 0.0826922)

    # bbr model summary
    mod_sum <- bbr::model_summary(mod)
    pathDF4 <- define_param_table(mod_sum, PARAM_KEY_DF)
    expect_equal(pathDF4$estimate[pathDF4$name == "OMEGA22"], 0.0826922)

    # bbr parameter estimates
    mod_est <- bbr::param_estimates(mod_sum)
    pathDF4 <- define_param_table(mod_est, PARAM_KEY_DF)
    expect_equal(pathDF4$estimate[pathDF4$name == "OMEGA22"], 0.0826922)
  })

  test_that("define_param_table handles multiple parameter key input types", {
    param_df <- define_param_table(MOD102_PATH, PARAM_KEY_PATH)
    expect_equal(param_df$estimate[param_df$name == "OMEGA22"], 0.0826922)

    key_df <- pmtables::yaml_as_df(PARAM_KEY_PATH_DF)
    param_df <- define_param_table(MOD102_PATH, key_df)
    expect_equal(param_df$estimate[param_df$name == "OMEGA22"], 0.0826922)
  })

  test_that("define_param_table incorrect parameter key input type", {
    # TODO: There should be a loadParamKey test file that tests this, rather
    # than having it here and in define_boot_table
    expect_warning(
      define_param_table(MOD102_PATH, PARAM_KEY_PATH_BOTH),
      "Only abb, desc, panel and trans arguments will be used"
    )
  })

  test_that("define_param_table generates correct corr_SD", {
    expect_true(all(PARAM_TAB_102$estimate == PARAM_TAB_102$value))
    expect_true(all(PARAM_TAB_102$stderr == PARAM_TAB_102$se))
    expect_true(PARAM_TAB_102$corr_SD[9] == "0.510933")
    expect_true(is.na(PARAM_TAB_102$corr_SD[1]))
    expect_true(is.na(PARAM_TAB_102$corr_SD[6]))
  })

  test_that("define_param_table generates the confidence intervals for various inputs", {
    PARAM_TAB_102_ci95 <- define_param_table(.estimates = PARAM_EST_102, .key = PARAM_KEY_DF, .ci = 95, .zscore = NULL)
    PARAM_TAB_102_ci90 <- define_param_table(.estimates = PARAM_EST_102, .key = PARAM_KEY_DF, .ci = 90, .zscore = NULL)

    expect_equal(PARAM_TAB_102_ci90$lower[1], 0.33047798)
    expect_equal(PARAM_TAB_102_ci90$upper[2], 4.1640688)

    expect_equal(PARAM_TAB_102_ci95$lower[4], 4.1721829)
    expect_equal(PARAM_TAB_102_ci95$upper[7], 0.10195012)
  })

  test_that("define_param_table expected dataframe: respects yaml key order",{
    paramKey2 <- PARAM_KEY_DF %>%
      filter(name %in% PARAM_TAB_102$name)

    expect_equal(PARAM_TAB_102$name, paramKey2$name)
  })

  test_that("define_param_table message if using theta error block", {
    expect_message(define_param_table(.estimates = theta_err, .key = theta_err_key, .ci = 95, .zscore = NULL))
  })


})
