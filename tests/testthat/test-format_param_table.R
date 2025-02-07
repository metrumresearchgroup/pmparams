newDF3 <- PARAM_TAB_102 %>%
  format_param_table()

newDF4 <- PARAM_TAB_102 %>%
  format_param_table(.prse = TRUE)

newDF5 <- PARAM_TAB_102 %>%
  format_param_table(.cleanup_cols = FALSE)

newDF6 <- THETA_ERR_PARAM_TAB %>%
  format_param_table(.cleanup_cols = FALSE, .prse = TRUE)

ci_name <- PARAM_TAB_102 %>% dplyr::distinct(ci_level) %>% dplyr::pull(ci_level)


test_that("format_param_table expected dataframe: col names", {
  # default cols., no prse
  expect_equal(names(newDF3),  c("type", "abb", "greek", "desc", "value", "shrinkage", paste0("ci_", ci_name)))

  # all cols., no prse
  expect_equal(length(names(newDF5)),  40)
})

test_that("format_param_table expected dataframe: prse col", {
  # default cols., prse
  expect_equal(newDF4$pRSE[1],  "6.29")
  expect_true("pRSE" %in% names(newDF6))

  expect_equal(length(names(newDF6)),  length(names(newDF5)))
  expect_true("pRSE" %in% names(newDF6))
})


test_that("format_param_table continuous columns expected ouput: CI back transforms", {
  newDF_log1 <- PARAM_TAB_102 %>%
    dplyr::mutate(
      ci = paste0(pmtables::sig(lower), ", ", pmtables::sig(upper))
    )

  expect_equal(newDF_log1$ci[5], "1.22, 1.36")
})

test_that("format_param_table continuous columns expected ouput: CI back transforms", {
  before_format <- PARAM_TAB_102 %>%
    mutate(ci_level = as.character(ci_level)) %>%
    distinct(ci_level) %>%
    pull(ci_level)

  after_format <- gsub("ci_", "", names(newDF3)[grepl("ci_", names(newDF3))], fixed = T)

  expect_equal(before_format, after_format)
})


test_that("format_param_table continuous columns expected ouput: shrinkage", {
  newDF_shrink <- PARAM_TAB_102 %>%
    dplyr::mutate(
      shrinkage = if_else(is.na(shrinkage), "-",
                          as.character(pmtables::sig(shrinkage))
      )
    )

  expect_equal(newDF_shrink$shrinkage[1], "-")
  expect_equal(newDF_shrink$shrinkage[6], "18.2")
  expect_equal(newDF_shrink$shrinkage[8], "0.898")
})

test_that("format_param_table continuous columns expected ouput: value", {
  expect_equal(newDF3$value[1], "1.54")
  expect_equal(newDF3$value[6], "0.221 [CV\\%=49.7]")
})

test_that("format_param_table continuous columns expected ouput: CI back transforms for addErrLogDV", {
  key_df <- pmtables::yaml_as_df(PARAM_KEY_PATH_DF) %>%
    filter(.row != "gg") %>%
    dplyr::add_row(.row = "gg", name = "SIGMA11", abb = "Lognormal residual error", desc = "Variance", panel = "RV", trans = "addErrLogDV")

  newDF4 <- define_param_table(.estimates = PARAM_EST_102, .key = key_df, .ci = 95, .zscore = NULL)

  newDF4a <- newDF4 %>% format_param_table()

  expected_cv =  pmtables::sig(sqrt(exp(newDF4$value[newDF4$addErrLogDV == TRUE]) -1)* 100)
  expected_value = paste0(pmtables::sig(newDF4$value[newDF4$addErrLogDV == TRUE]), " [CV\\%=", expected_cv, "]")

  expect_equal(newDF4a$value[newDF4a$abb == "Lognormal residual error"], expected_value)
})

test_that("format_param_table continuous columns expected ouput: greek", {
  expect_equal(newDF5$greek[newDF5$S & !newDF5$THETAERR], "$\\Sigma_{(1,1)}$")
  expect_equal(newDF6$greek[newDF6$S & newDF6$THETAERR], "$\\theta_{(1,1)}$")
})

test_that("format_param_table expected dataframe: respects yaml key order", {
  expect_equal(unname(unlist(PARAM_KEY_YAML))[grepl('desc',names(unlist(PARAM_KEY_YAML)),fixed=T) & unlist(PARAM_KEY_YAML) %in% newDF3$desc],
               newDF3$desc)
})

test_that("format_param_table panel expected ouput", {
  paramKey1 <- PARAM_KEY_DF %>%
    mutate(
      panel = if_else(name == "THETA2", "IIV", panel),
      trans = if_else(name == "THETA2", "none", trans)
    )

  newDF7 <- define_param_table(.estimates = PARAM_EST_102, .key = paramKey1, .ci = 95, .zscore = NULL)
  newDF8 <- format_param_table(newDF7, .cleanup_cols = FALSE)

  expect_equal(newDF8$type[newDF8$name == "THETA2"], "Interindividual variance parameters")
  expect_equal(newDF8$type_f[newDF8$name == "THETA2"], 3)
  expect_equal(newDF8$panel[newDF8$name == "THETA2"], "IIV")
})

test_that("format_param_table: logit back transform large estimate returns 1", {
  newDF9 <- data.frame(LOGIT = TRUE, value = 1e6, lower = 2, upper = 1e10)
  newDF10 <- backTrans_logit(newDF9)
  expect_equal(newDF10$value, 1)
  expect_equal(newDF10$lower, stats::plogis(newDF9$lower))
  expect_equal(newDF10$upper, 1)
})


test_that("format_param_table: .digit produces expected significant digits", {

  newDF9 <- format_param_table(PARAM_TAB_102)
  expect_equal(newDF9$value[9], "0.0690 [Corr=0.511]")
  expect_equal(newDF9$ci_95[9], "0.0299, 0.108")
  expect_equal(newDF9$shrinkage[6], "18.2")

  newDF10 <- format_param_table(PARAM_TAB_102, .digit = 6)
  expect_equal(newDF10$value[9], "0.0690088 [Corr=0.510933]")
  expect_equal(newDF10$ci_95[9], "0.0298846, 0.108133")
  expect_equal(newDF10$shrinkage[6], "18.1558")
})

test_that("format_param_table: .maxex produces expected scientific notation", {

  newDF11 <- PARAM_TAB_102 %>%
    mutate(value = value*100,
           upper = upper*0.01,
           lower = lower*0.0001)

  newDF12 <- format_param_table(newDF11)
  expect_equal(newDF12$value[1], "6.77e+18")
  expect_equal(newDF12$value[10], "13.4 [Corr=0.694]")
  expect_equal(newDF12$ci_95[10], "8.78e-06, 0.00180")
  expect_equal(newDF12$shrinkage[6], "18.2")
})

test_that("format_param_table: .select_cols works", {
  rlang::local_options(lifecycle_verbosity = "warning")
  # Traditional use works
  expect_warning(
    df2 <- format_param_table(PARAM_TAB_102, .select_cols = c("abb", "ci_95")),
    "is deprecated"
  )
  expect_equal(names(df2), c("abb", "ci_95"))

  # 'all' returns all columns
  expect_warning(
    df2 <- format_param_table(PARAM_TAB_102, .select_cols = "ALL"),
    "is deprecated"
  )
  expect_equal(setdiff(names(df2), names(PARAM_TAB_102)), c("cv", "pRSE", "sd", "text", "greek", "type", "type_f", "ci_95"))

  # Missing cols
  expect_error(
    format_param_table(PARAM_TAB_102, .select_cols = "other") %>% suppressWarnings(),
    "The following specified columns were not found: other"
  )

  # Old way of selecting `ci` column messages
  expect_message(
    df2 <- format_param_table(PARAM_TAB_102, .select_cols = "ci") %>% suppressWarnings(),
    "`ci` is no longer a valid column name. pmparams will select ci_95"
  )

})
