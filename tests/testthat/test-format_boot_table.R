
newDF5 <- BOOT_TAB_106 %>%
  format_boot_table(.cleanup_cols = FALSE)


test_that("format_boot_table expected dataframe: col names", {
  # default cols
  expect_equal(names(FMT_BOOT_TAB_106),  c("abb", "desc", "boot_value", "boot_ci_95"))

  # all cols
  expect_equal(length(setdiff(names(BOOT_TAB_106), names(newDF5))), 0)
  expect_equal(setdiff(names(newDF5), names(BOOT_TAB_106)), c("boot_value", "boot_ci_95"))
})



test_that("format_param_table continuous columns expected ouput: value", {
  expect_equal(FMT_BOOT_TAB_106$boot_value[1], "1.57")
  expect_equal(FMT_BOOT_TAB_106$boot_ci_95[3], "3.07, 3.42")
  expect_equal(FMT_BOOT_TAB_106$boot_value[6], "0.484")
})

test_that("format_boot_table expected dataframe: respects yaml key order", {
  expect_equal(unname(unlist(PARAM_KEY_YAML))[grepl('desc',names(unlist(PARAM_KEY_YAML)),fixed=T) & unlist(PARAM_KEY_YAML) %in% FMT_BOOT_TAB_106$desc],
               FMT_BOOT_TAB_106$desc)
})

test_that("format_param_table: .digit produces expected significant digits", {

  expect_equal(FMT_BOOT_TAB_106$boot_value[6], "0.484")
  expect_equal(FMT_BOOT_TAB_106$boot_ci_95[6], "0.408, 0.558")

  newDF4 <- format_boot_table(BOOT_TAB_106, .digit = 6)
  expect_equal(newDF4$boot_value[6], "0.483687")
  expect_equal(newDF4$boot_ci_95[6], "0.408194, 0.558204")
})

test_that("format_param_table: .maxex produces expected scientific notation", {

  newDF5 <- BOOT_TAB_106 %>%
    mutate(value = value*100,
           upper = upper*0.01,
           lower = lower*0.0001)

  newDF6 <- format_boot_table(newDF5)
  expect_equal(newDF6$boot_value[1], "157")
  expect_equal(newDF6$boot_value[10], "8.21")
  expect_equal(newDF6$boot_ci_95[1], "0.000139, 0.0178")
  expect_equal(newDF6$boot_ci_95[6], "4.08e-05, 0.00558")

  newDF7 <- format_boot_table(newDF5, .maxex = 99)
  expect_equal(newDF7$boot_value[1], "157")
  expect_equal(newDF7$boot_value[10], "8.21")
  expect_equal(newDF7$boot_ci_95[1], "0.000139, 0.0178")
  expect_equal(newDF7$boot_ci_95[6], "0.0000408, 0.00558")

  # maxex and digit
  newDF8 <- format_boot_table(newDF5, .digit = 2, .maxex = 99)
  expect_equal(newDF8$boot_value[1], "160")
  expect_equal(newDF8$boot_value[10], "8.2")
  expect_equal(newDF8$boot_ci_95[1], "0.00014, 0.018")
  expect_equal(newDF8$boot_ci_95[6], "0.000041, 0.0056")

})

test_that("format_boot_table: CI not calculated for fixed parameters", {
  skip_if_missing_deps("bbr", "1.11.0")
  boot_df <- define_boot_table(
    bbr::bootstrap_estimates(BOOT_RUN), .key = PARAM_KEY_DF
  )
  boot_df_fmt <- format_boot_table(boot_df)
  expect_equal(
    boot_df_fmt$boot_ci_95[boot_df_fmt$abb == "Proportional"],
    "FIXED"
  )
})

test_that("format_boot_table: .select_cols works", {
  df1 <- define_boot_table(BOOT_106_EST, .key = PARAM_KEY_DF, .ci = "iqr")

  expect_message(
    df2 <- format_boot_table(df1, .select_cols = c("lower", "desc")),
    "`.select_cols` is deprecated"
  )
  expect_equal(names(df2), c("lower", "desc"))

  # 'all' returns all columns
  expect_message(
    df2 <- format_boot_table(df1, .select_cols = "ALL"),
    "`.select_cols` is deprecated"
  )
  expect_equal(setdiff(names(df2), names(df1)), c("boot_value", "boot_ci_50"))

  expect_error(
    format_boot_table(df1, .select_cols = "other") %>% suppressMessages(),
    "The following specified columns were not found: other"
  )
})
