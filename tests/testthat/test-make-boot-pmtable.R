
test_that("make_boot_pmtable pmtable commands: st_panel", {
  pm_tibble1 <- make_boot_pmtable(BOOT_PARAM_TAB_106)
  expect_equal(unname(unlist(pm_tibble1$panel$col)), "type")
})

test_that("make_boot_pmtable pmtable commands: make cols blank", {
  pm_tibble1 <- make_boot_pmtable(BOOT_PARAM_TAB_106, .pmtype = "full")
  expect_equal(unname(unlist(pm_tibble1$cols_blank)), c("abb", "greek", "desc"))

  pm_tibble2 <- make_boot_pmtable(BOOT_PARAM_TAB_106, .pmtype = "random")
  expect_equal(unname(unlist(pm_tibble2$cols_blank)), c("abb", "greek"))
})

test_that("make_boot_pmtable pmtable commands: rename cols", {
  pm_tibble3 <- make_boot_pmtable(BOOT_PARAM_TAB_106, .pmtype = "structural")
  expect_equal(unname(unlist(pm_tibble3$cols_rename[1])), "value")

  pm_tibble4 <- make_boot_pmtable(BOOT_PARAM_TAB_106, .pmtype = "random")
  expect_equal(unname(unlist(pm_tibble4$cols_rename)), c("value", "shrinkage", "pRSE"))
})


test_that("make_boot_pmtable correctly filters with .pmtype", {
  pm_tibble4 <- make_boot_pmtable(BOOT_PARAM_TAB_106, .pmtype = "random")
  pm_tibble5 <- make_boot_pmtable(BOOT_PARAM_TAB_106, .pmtype = "fixed")
  pm_tibble6 <- make_boot_pmtable(BOOT_PARAM_TAB_106, .pmtype = "full")
  pm_tibble7 <- make_boot_pmtable(BOOT_PARAM_TAB_106, .pmtype = "structural")

  # random
  expect_equal(7, nrow(pm_tibble4$data))
  # fixed
  expect_equal(8, nrow(pm_tibble5$data))
  # full
  expect_equal(15, nrow(pm_tibble6$data))
  # structural
  expect_equal(5, nrow(pm_tibble7$data))
})


test_that("make_boot_pmtable: incorrect dataframe checks", {
  # Missing required columns
  expect_error(
    make_boot_pmtable(FMT_BOOT_TAB_106),
    "The following required columns are missing: type, greek, value, shrinkage"
  )

  # Missing bootstrap estimates
  expect_error(
    make_boot_pmtable(FMT_PARAM_TAB_106),
    "No confidence intervals or percentiles for the bootstrap parameter estimates were detected"
  )
})


test_that("make_boot_pmtable formats spanned columns correctly", {
  model_label = "model stuff"
  boot_label = "bootstrap stuff"

  pm_tibble <- make_boot_pmtable(
    BOOT_PARAM_TAB_106,
    .span_model_label = model_label,
    .span_boot_label = boot_label
  )

  expect_equal(pm_tibble$span[[1]]$title, model_label)
  expect_equal(
    names(pm_tibble$data %>% dplyr::select(!!pm_tibble$span[[1]]$vars)),
    c("value", "shrinkage")
  )
  expect_equal(pm_tibble$span[[2]]$title, boot_label)
  expect_equal(
    names(pm_tibble$data %>% dplyr::select(!!pm_tibble$span[[2]]$vars)),
    c("Median", "95\\% CI")
  )

  # RSE % column is grouped when included
  param_df <- format_param_table(PARAM_TAB_106, .prse = TRUE)
  combine_df <- left_join(param_df, FMT_BOOT_TAB_106, by = c("abb", "desc"))
  pm_tibble <- make_boot_pmtable(combine_df)
  expect_equal(
    names(pm_tibble$data %>% dplyr::select(!!pm_tibble$span[[1]]$vars)),
    c("value", "shrinkage", "pRSE")
  )

  # CI column from original model is grouped when included
  pm_tibble <- make_boot_pmtable(BOOT_PARAM_TAB_106, .drop_model_ci = FALSE)
  expect_equal(
    names(pm_tibble$data %>% dplyr::select(!!pm_tibble$span[[1]]$vars)),
    c("value", "ci_95", "shrinkage")
  )
})


test_that("make_boot_pmtable drops or renames ci_[x] columns", {
  # Test dropping
  pm_tibble <- make_boot_pmtable(BOOT_PARAM_TAB_106)
  expect_false("ci_95" %in% names(pm_tibble))

  # Test renaming
  # - Renaming stored in pmtables object and only happens when rendering
  # - This is to avoid having columns with the same name appear twice and error out
  pm_tibble <- make_boot_pmtable(
    BOOT_PARAM_TAB_106,
    .drop_model_ci = FALSE
  )
  expect_true("ci_95" %in% names(pm_tibble))

  expect_identical(
    unname(pm_tibble$cols_rename),
    c("value", "ci_95", "shrinkage", "pRSE")
  )

  expect_identical(
    names(pm_tibble$cols_rename),
    c("Estimate", "95\\% CI", "Shrinkage (\\%)", "RSE (\\%)")
  )
})
