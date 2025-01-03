
newDF5 <- BOOT_TAB_106 %>%
  format_boot_table(.cleanup_cols = FALSE)


test_that("format_boot_table expected dataframe: col names", {
  # default cols
  expect_equal(names(FMT_BOOT_TAB_106),  c("abb", "desc", "boot_median", "boot_ci_95"))

  # all cols
  expect_equal(setdiff(names(BOOT_TAB_106), names(newDF5)), c("boot_perc_2.5", "boot_perc_50", "boot_perc_97.5"))
  expect_equal(setdiff(names(newDF5), names(BOOT_TAB_106)), c("boot_median", "boot_ci_95"))
})



test_that("format_boot_table: .digit produces expected significant digits", {

  expect_equal(FMT_BOOT_TAB_106$boot_median[6], "0.484")
  expect_equal(FMT_BOOT_TAB_106$boot_ci_95[6], "0.408, 0.558")

  newDF4 <- format_boot_table(BOOT_TAB_106, .digit = 6)
  expect_equal(newDF4$boot_median[6], "0.483687")
  expect_equal(newDF4$boot_ci_95[6], "0.408194, 0.558204")
})

test_that("format_boot_table: .maxex produces expected scientific notation", {

  newDF5 <- BOOT_TAB_106 %>%
    mutate(boot_perc_50 = boot_perc_50*100,
           boot_perc_97.5 = boot_perc_97.5*0.01,
           boot_perc_2.5 = boot_perc_2.5*0.0001)

  newDF6 <- format_boot_table(newDF5)
  expect_equal(newDF6$boot_median[1], "157")
  expect_equal(newDF6$boot_median[10], "8.21")
  expect_equal(newDF6$boot_ci_95[1], "0.000139, 0.0178")
  expect_equal(newDF6$boot_ci_95[6], "4.08e-05, 0.00558")

  newDF7 <- format_boot_table(newDF5, .maxex = 99)
  expect_equal(newDF7$boot_median[1], "157")
  expect_equal(newDF7$boot_median[10], "8.21")
  expect_equal(newDF7$boot_ci_95[1], "0.000139, 0.0178")
  expect_equal(newDF7$boot_ci_95[6], "0.0000408, 0.00558")

  # maxex and digit
  newDF8 <- format_boot_table(newDF5, .digit = 2, .maxex = 99)
  expect_equal(newDF8$boot_median[1], "160")
  expect_equal(newDF8$boot_median[10], "8.2")
  expect_equal(newDF8$boot_ci_95[1], "0.00014, 0.018")
  expect_equal(newDF8$boot_ci_95[6], "0.000041, 0.0056")
})

test_that("format_boot_table: outputs individual percentiles", {
  # Arbitrary percentiles that dont have any specific handling and cant
  # be grouped
  percents <- c(0.1, 0.55, 0.36, 0.98, 0.77)

  df1 <- define_boot_table(
    BOOT_106_EST, .key = PARAM_KEY_DF, .percentiles = percents
  )
  df2 <- format_boot_table(df1)

  perc_fmt <- paste0("boot_perc_", sort(percents*100))
  expect_equal(names(df2), c("abb","desc", perc_fmt))
})

test_that("format_boot_table: percentiles are appropriately grouped", {
  # Test 3 groupings: 90, 95, and iqr
  percents <- c(0.025, 0.05, 0.5, 0.95, 0.975,  0.25, 0.75)

  df1 <- define_boot_table(
    BOOT_106_EST, .key = PARAM_KEY_DF, .percentiles = percents
  )
  df2 <- format_boot_table(df1)

  grouped_cols <- paste0("boot_ci_", c("iqr", "90", "95"))
  expect_equal(names(df2), c("abb","desc", "boot_median", grouped_cols))

  # Test 2 groupings and individual percent
  percents <- c(0.025, 0.05, 0.5, 0.95, 0.975, 0.25)

  df1 <- define_boot_table(
    BOOT_106_EST, .key = PARAM_KEY_DF, .percentiles = percents
  )
  df2 <- format_boot_table(df1)

  grouped_cols <- paste0("boot_ci_", c("90", "95"))
  indiv_cols <- c("boot_perc_25", "boot_median")
  expect_equal(names(df2), c("abb","desc", indiv_cols, grouped_cols))
})

test_that("format_boot_table: iqr and median have their own handling", {
  # Test via iqr
  df1 <- define_boot_table(
    BOOT_106_EST, .key = PARAM_KEY_DF, .ci = "iqr"
  )
  df2 <- format_boot_table(df1)

  boot_cols <- paste0("boot_", c("median", "ci_iqr"))
  expect_equal(names(df2), c("abb","desc", boot_cols))

  # Test via percentiles
  df1 <- define_boot_table(
    BOOT_106_EST, .key = PARAM_KEY_DF, .percentiles = c(.25, .50, .75)
  )
  df2 <- format_boot_table(df1)

  boot_cols <- paste0("boot_", c("median", "ci_iqr"))
  expect_equal(names(df2), c("abb","desc", boot_cols))
})


test_that("format_boot_table: .select_cols works", {
  df1 <- define_boot_table(BOOT_106_EST, .key = PARAM_KEY_DF, .ci = "iqr")

  expect_message(
    df2 <- format_boot_table(df1, .select_cols = c("boot_perc_25", "desc")),
    "`.select_cols` is deprecated"
  )
  expect_equal(names(df2), c("boot_perc_25", "desc"))

  # 'all' returns all columns
  expect_message(
    df2 <- format_boot_table(df1, .select_cols = "ALL"),
    "`.select_cols` is deprecated"
  )
  expect_equal(setdiff(names(df2), names(df1)), c("boot_median", "boot_ci_iqr"))

  expect_error(
    format_boot_table(df1, .select_cols = "other") %>% suppressMessages(),
    "The following specified columns were not found: other"
  )
})
