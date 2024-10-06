newDF3 <- newbootDF %>%
  format_boot_table()

newDF5 <- newbootDF %>%
  format_boot_table(.cleanup_cols = FALSE)


test_that("format_boot_table expected dataframe: col names", {
  #default cols
  expect_equal(names(newDF3),  c("panel", "abb", "desc", "boot_value_50", "boot_perc_95"))

  #all cols
  expect_equal(length(names(newDF5)),  24)
})



test_that("format_param_table continuous columns expected ouput: value", {
  expect_equal(newDF3$boot_value_50[1], "1.57")
  expect_equal(newDF3$boot_perc_95[3], "3.07, 3.42")
  expect_equal(newDF3$boot_value_50[6], "0.484")
})

test_that("format_boot_table expected dataframe: respects yaml key order", {
  expect_equal(unname(unlist(param_yaml))[grepl('desc',names(unlist(param_yaml)),fixed=T) & unlist(param_yaml) %in% newDF3$desc],
               newDF3$desc)
})

test_that("format_param_table: .digit produces expected significant digits", {

  expect_equal(newDF3$boot_value_50[6], "0.484")
  expect_equal(newDF3$boot_perc_95[6], "0.408, 0.558")

  newDF4 <- format_boot_table(newbootDF, .digit = 6)
  expect_equal(newDF4$boot_value_50[6], "0.483687")
  expect_equal(newDF4$boot_perc_95[6], "0.408194, 0.558204")
})

test_that("format_param_table: .maxex produces expected scientific notation", {

  newDF5 <- newbootDF %>%
    mutate(value_perc50 = value_perc50*100,
           upper_perc97.5 = upper_perc97.5*0.01,
           lower_perc2.5 = lower_perc2.5*0.0001)

  newDF6 <- format_boot_table(newDF5)
  expect_equal(newDF6$boot_value_50[1], "157")
  expect_equal(newDF6$boot_value_50[10], "8.21")
  expect_equal(newDF6$boot_perc_95[1], "0.000139, 0.0178")
  expect_equal(newDF6$boot_perc_95[6], "4.08e-05, 0.00558")

  newDF7 <- format_boot_table(newDF5, .maxex = 99)
  expect_equal(newDF7$boot_value_50[1], "157")
  expect_equal(newDF7$boot_value_50[10], "8.21")
  expect_equal(newDF7$boot_perc_95[1], "0.000139, 0.0178")
  expect_equal(newDF7$boot_perc_95[6], "0.0000408, 0.00558")

  #maxex and digit
  newDF8 <- format_boot_table(newDF5, .digit = 2, .maxex = 99)
  expect_equal(newDF8$boot_value_50[1], "160")
  expect_equal(newDF8$boot_value_50[10], "8.2")
  expect_equal(newDF8$boot_perc_95[1], "0.00014, 0.018")
  expect_equal(newDF8$boot_perc_95[6], "0.000041, 0.0056")

})

test_that("format_param_table: outputs all percentiles", {
 df1 <- define_boot_table(.boot_estimates =boot_paramEst,
                          .key = paramKey,
                          .percentiles = c(0.1, 0.55, 0.36, 0.98, 0.77))
 df2 <- format_boot_table(df1)

 expect_equal(names(df2),
              c("panel", "abb","desc", "perc10", "perc36", "perc55", "perc77", "perc98")
            )

 expect_message(format_boot_table(df1, .select_cols =c("perc10", "desc")))

 df3 <- format_boot_table(df1,
                          .select_cols =c("perc10", "desc"))

 expect_equal(names(df3),
              c("perc10", "desc")
 )
})


##ADD tests for new cases###
#
#select_cols
# newDF7 <- newDF5 %>%
#   format_boot_table(.cleanup_cols = TRUE,
#                      .select_cols = "ALL")
#
# newDF8 <- newDF5 %>%
#   format_boot_table(.cleanup_cols = TRUE,
#                      .select_cols = c("panel", "abb", "greek", "desc", "value", "ci"))
#
# newDF9 <- newDF5 %>%
#   format_boot_table(.cleanup_cols = TRUE,
#                      .select_cols = c("other"))

## multiple percentiles
#define_boot_table()
