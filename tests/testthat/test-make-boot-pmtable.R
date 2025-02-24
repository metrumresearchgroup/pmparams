
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
