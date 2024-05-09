newDF3 <- newDF %>%
  format_param_table()

newDF4 <- newDF %>%
  format_param_table(.prse = TRUE)

newDF5 <- newDF %>%
  format_param_table(.select_cols = "all")

newDF6 <- theta_err_df1 %>%
  format_param_table(.select_cols = "all", .prse = TRUE)

ci_name <- newDF %>% dplyr::distinct(ci_level) %>% dplyr::pull(ci_level)


test_that("format_param_table expected dataframe: col names", {
  #default cols., no prse
  expect_equal(names(newDF3),  c("type", "abb", "greek", "desc", "value", paste0("ci_", ci_name),"shrinkage"))

  #all cols., no prse
  expect_equal(length(names(newDF5)),  42)
})

test_that("format_param_table expected dataframe: prse col", {
  #default cols., prse
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

  newDF5 <- newDF4%>% format_param_table()

  expected_cv =  pmtables::sig(sqrt(exp(newDF4$value[newDF4$addErrLogDV == TRUE]) -1)* 100)
  expected_value = paste0(pmtables::sig(newDF4$value[newDF4$addErrLogDV == TRUE]), " [CV\\%=", expected_cv, "]")

  expect_equal(newDF5$value[newDF5$abb == "Lognormal residual error"], expected_value)
})

test_that("format_param_table continuous columns expected ouput: greek", {
  expect_equal(newDF5$greek[newDF5$S & !newDF5$THETAERR], "$\\Sigma_{(1,1)}$")
  expect_equal(newDF6$greek[newDF6$S & newDF6$THETAERR], "$\\theta_{(1,1)}$")
})

test_that("format_param_table expected dataframe: respects yaml key order", {
  expect_equal(unname(unlist(param_yaml))[grepl('desc',names(unlist(param_yaml)),fixed=T) & unlist(param_yaml) %in% newDF3$desc],
               newDF3$desc)
})
