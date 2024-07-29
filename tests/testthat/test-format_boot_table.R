newDF3 <- newbootDF %>%
  format_boot_table()

newDF5 <- newbootDF %>%
  format_boot_table(.select_cols = "all")


test_that("format_boot_table expected dataframe: col names", {
  #default cols
  expect_equal(names(newDF3),  c("abb", "desc", "boot_value", "boot_ci_95"))

  #all cols
  expect_equal(length(names(newDF5)),  26)
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

test_that("format_param_table: .digit produces expected significant digits", {

  expect_equal(newDF3$boot_value[6], "0.484")
  expect_equal(newDF3$boot_ci_95[6], "0.408, 0.558")

  newDF4 <- format_boot_table(newbootDF, .digit = 6)
  expect_equal(newDF4$boot_value[6], "0.483687")
  expect_equal(newDF4$boot_ci_95[6], "0.408194, 0.558204")
})

test_that("format_param_table: .maxex produces expected scientific notation", {

  newDF5 <- newbootDF %>%
    mutate(estimate = estimate*100,
           upper = upper*0.01,
           lower = lower*0.0001)

  newDF6 <- format_boot_table(newDF5)
  expect_equal(newDF6$boot_value[1], "1.57")
  expect_equal(newDF6$boot_value[10], "0.0821")
  expect_equal(newDF6$boot_ci_95[1], "0.000139, 0.0178")
  expect_equal(newDF6$boot_ci_95[6], "4.08e-05, 0.00558")

  newDF7 <- format_boot_table(newDF5, .maxex = 99)
  expect_equal(newDF7$boot_value[1], "1.57")
  expect_equal(newDF7$boot_value[10], "0.0821")
  expect_equal(newDF7$boot_ci_95[1], "0.000139, 0.0178")
  expect_equal(newDF7$boot_ci_95[6], "0.0000408, 0.00558")

  #maxex and digit
  newDF8 <- format_boot_table(newDF5, .digit = 2, .maxex = 99)
  expect_equal(newDF8$boot_value[1], "1.6")
  expect_equal(newDF8$boot_value[10], "0.082")
  expect_equal(newDF8$boot_ci_95[1], "0.00014, 0.018")
  expect_equal(newDF8$boot_ci_95[6], "0.000041, 0.0056")

})
