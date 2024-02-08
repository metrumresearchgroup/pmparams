
test_that("make_pmtable incorrect input type: invalid .pmtype",{
  expect_error(capture.output(make_pmtable(.df = newFormatDF, .pmtype = "no type")))
})

test_that("make_pmtable pmtable commands: st_panel", {
  pm_tibble1 <- make_pmtable(.df = newFormatDF, .pmtype = "full")
  expect_equal(unname(unlist(pm_tibble$panel$col)), "type")
})

test_that("make_pmtable pmtable commands: make cols blank", {
  pm_tibble1 <- make_pmtable(.df = newFormatDF, .pmtype = "full")
  expect_equal(unname(unlist(pm_tibble1$cols_blank)), c("abb", "greek", "desc"))

  pm_tibble2 <- make_pmtable(.df = newFormatDF, .pmtype = "random")
  expect_equal(unname(unlist(pm_tibble2$cols_blank)), c("abb", "greek"))
})

test_that("make_pmtable pmtable commands: rename cols", {
  pm_tibble3 <- make_pmtable(.df = newFormatDF, .pmtype = "fixed structural")
  expect_equal(unname(unlist(pm_tibble1$cols_rename)), "value")

  pm_tibble4 <- make_pmtable(.df = newFormatDF, .pmtype = "random")
  expect_equal(unname(unlist(pm_tibble4$cols_rename)), c("value", "shrinkage"))
})


test_that("make_pmtable pmtable commands: notes", {
  pm_tibble5 <- make_pmtable(.df = newFormatDF, .pmtype = "fixed", .notes = c("note 1", "note2"))
  pm_tibble6 <- make_pmtable(.df = newFormatDF, .pmtype = "fixed", .notes = c("note 1", "note2"), .width = 2.1)

  expect_equal(pm_tibble5$notes, c("note 1", "note2"))
  expect_equal(pm_tibble5$note_config$width, 1)
  expect_equal(pm_tibble6$note_config$width, 2.1)
})

test_that("make_pmtable pmtable commands: args", {
  expect_equal(unlist(pm_tibble5$args), NULL)
})


test_that("make_pmtable correctly filters with .pmtype", {

  #random
  expect_equal(
    pm_tibble4$data %>%
      filter(
        stringr::str_detect(greek, "Omega") |
          stringr::str_detect(type, "Resid")
      ) %>%
      nrow(),
    nrow(pm_tibble4$data)
  )

  #fixed
  expect_equal(
    pm_tibble5$data %>%
      filter(
        stringr::str_detect(pm_tibble5$data$type, "Struct") |
          stringr::str_detect(pm_tibble5$data$type, "effect")
      ) %>%
      nrow(),
    nrow(pm_tibble5$data)
  )
})
