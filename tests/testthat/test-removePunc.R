df_ex <- dplyr::tribble(~ parameter_names, "THETA1", "OMEGA(1,1)", "OMEGA(2,2)")

test_that("removePunc removes paranetheses from string vector", {
  expect_equal(removePunc(df_ex, "parameter_names")$name[1], "THETA1")
  expect_equal(removePunc(df_ex, "parameter_names")$name[2], "OMEGA11")
  expect_equal(removePunc(df_ex, "parameter_names")$name[3], "OMEGA22")
})
