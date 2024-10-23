newDF3 <- newDF %>%
  format_param_table()

newDF4 <- newDF %>%
  format_param_table(.prse = TRUE)

newDF5 <- newDF %>%
  format_param_table(.select_cols = "all") %>% suppressMessages()

newDF6 <- theta_err_df1 %>%
  format_param_table(.select_cols = "all", .prse = TRUE) %>% suppressMessages()

ci_name <- newDF %>% dplyr::distinct(ci_level) %>% dplyr::pull(ci_level)


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
  newDF_log1 <- newDF %>%
    dplyr::mutate(
      ci = paste0(pmtables::sig(lower), ", ", pmtables::sig(upper))
    )

  expect_equal(newDF_log1$ci[5], "1.22, 1.36")
})

test_that("format_param_table continuous columns expected ouput: CI back transforms", {
  before_format <- newDF %>%
    mutate(ci_level = as.character(ci_level)) %>%
    distinct(ci_level) %>%
    pull(ci_level)

  after_format <- gsub("ci_", "", names(newDF3)[grepl("ci_", names(newDF3))], fixed = T)

  expect_equal(before_format, after_format)
})


test_that("format_param_table continuous columns expected ouput: shrinkage", {
  newDF_shrink <- newDF %>%
    dplyr::mutate(
      shrinkage = if_else(is.na(shrinkage), "-",
                          as.character(pmtables::sig(shrinkage))
      )
    )

  expect_equal(newDF_shrink$shrinkage[1], "-")
  expect_equal(newDF_shrink$shrinkage[6], "17.9")
  expect_equal(newDF_shrink$shrinkage[8], "0.587")
})

test_that("format_param_table continuous columns expected ouput: value", {
  expect_equal(newDF3$value[1], "1.54")
  expect_equal(newDF3$value[6], "0.221 [CV\\%=49.7]")
})

test_that("format_param_table continuous columns expected ouput: CI back transforms for addErrLogDV", {
  key_file <- system.file("model/nonmem/pk-parameter-key.yaml", package = "pmparams")
  key_df <- pmtables::yaml_as_df(key_file) %>%
    filter(.row != "gg") %>%
    dplyr::add_row(.row = "gg", name = "SIGMA11", abb = "Lognormal residual error", desc = "Variance", panel = "RV", trans = "addErrLogDV")

  newDF4 <- define_param_table(.estimates = paramEst, .key = key_df, .ci = 95, .zscore = NULL)

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
  expect_equal(unname(unlist(param_yaml))[grepl('desc',names(unlist(param_yaml)),fixed=T) & unlist(param_yaml) %in% newDF3$desc],
               newDF3$desc)
})

test_that("format_param_table panel expected ouput", {
  paramKey1 <- paramKey %>%
    mutate(
      panel = if_else(name == "THETA2", "IIV", panel),
      trans = if_else(name == "THETA2", "none", trans)
    )

  newDF7 <- define_param_table(.estimates = paramEst, .key = paramKey1, .ci = 95, .zscore = NULL)
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

  newDF9 <- format_param_table(newDF)
  expect_equal(newDF9$value[9], "0.0690 [Corr=0.511]")
  expect_equal(newDF9$ci_95[9], "0.0299, 0.108")
  expect_equal(newDF9$shrinkage[6], "17.9")

  newDF10 <- format_param_table(newDF, .digit = 6)
  expect_equal(newDF10$value[9], "0.0690088 [Corr=0.510933]")
  expect_equal(newDF10$ci_95[9], "0.0298839, 0.108134")
  expect_equal(newDF10$shrinkage[6], "17.8988")
})

test_that("format_param_table: .maxex produces expected scientific notation", {

  newDF11 <- newDF %>%
    mutate(value = value*100,
           upper = upper*0.01,
           lower = lower*0.0001)

  newDF12 <- format_param_table(newDF11)
  expect_equal(newDF12$value[1], "6.77e+18")
  expect_equal(newDF12$value[10], "13.4 [Corr=0.694]")
  expect_equal(newDF12$ci_95[10], "8.78e-06, 0.00180")
  expect_equal(newDF12$shrinkage[6], "17.9")
})

test_that("format_param_table: .select_cols works", {

  # Traditional use works
  expect_message(
    df2 <- format_param_table(newDF, .select_cols = c("abb", "ci_95")),
    "`.select_cols` is deprecated"
  )
  expect_equal(names(df2), c("abb", "ci_95"))

  # 'all' returns all columns
  expect_message(
    df2 <- format_param_table(newDF, .select_cols = "ALL"),
    "`.select_cols` is deprecated"
  )
  expect_equal(setdiff(names(df2), names(newDF)), c("cv", "pRSE", "sd", "text", "greek", "type", "type_f", "ci_95"))

  # Missing cols
  expect_error(
    format_param_table(newDF, .select_cols = "other") %>% suppressMessages(),
    "The following specified columns were not found: other"
  )
})
