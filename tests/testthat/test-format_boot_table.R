newDF3 <- newbootDF %>%
  format_boot_table()

newDF5 <- newbootDF %>%
  format_boot_table(.select_cols = "all")


test_that("format_boot_table expected dataframe: col names", {
  #default cols
  expect_equal(names(newDF3),  c("abb", "desc", "boot_value", "boot_ci"))

  #all cols
  expect_equal(length(names(newDF5)),  24)
})



test_that("format_param_table continuous columns expected ouput: value", {
  expect_equal(newDF3$boot_value[1], "1.57")
  expect_equal(newDF3$boot_ci[3], "3.07, 3.42")
  expect_equal(newDF3$boot_value[6], "0.484")
})

