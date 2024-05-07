newDF3 <- newbootDF %>%
  format_boot_table()

newDF5 <- newbootDF %>%
  format_boot_table(.select_cols = "all")


test_that("format_boot_table expected dataframe: col names", {
  #default cols
  expect_equal(names(newDF3),  c("abb", "desc", "boot_value", "boot_ci_95"))

  #all cols
  expect_equal(length(names(newDF5)),  25)
})



test_that("format_param_table continuous columns expected ouput: value", {
  expect_equal(newDF3$boot_value[1], "1.57")
  expect_equal(newDF3$boot_ci_95[3], "3.07, 3.42")
  expect_equal(newDF3$boot_value[6], "0.484")
})

test_that("format_boot_table expected dataframe: respects yaml key order", {
  expect_equal(unname(unlist(param_yaml))[grepl('desc',names(unlist(param_yaml)),fixed=T) & unlist(param_yaml) %in% newDF3$desc],
               newDF3$desc)
})

