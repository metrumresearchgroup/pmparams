
test_that("make_pmtable incorrect input type: invalid .pmtype",{
  expect_error(capture.output(make_pmtable(.df = newFormatDF, .pmtype = "no type")))
})

test_that("make_pmtable pmtable commands: st_panel", {
  pm_tibble1 <- make_pmtable(.df = newFormatDF, .pmtype = "full")
  expect_equal(unname(unlist(pm_tibble1$panel$col)), "type")
})

test_that("make_pmtable pmtable commands: make cols blank", {
  pm_tibble1 <- make_pmtable(.df = newFormatDF, .pmtype = "full")
  expect_equal(unname(unlist(pm_tibble1$cols_blank)), c("abb", "greek", "desc"))

  pm_tibble2 <- make_pmtable(.df = newFormatDF, .pmtype = "random")
  expect_equal(unname(unlist(pm_tibble2$cols_blank)), c("abb", "greek"))
})

test_that("make_pmtable pmtable commands: rename cols", {
  pm_tibble3 <- make_pmtable(.df = newFormatDF, .pmtype = "structural")
  expect_equal(unname(unlist(pm_tibble3$cols_rename)), "value")

  pm_tibble4 <- make_pmtable(.df = newFormatDF, .pmtype = "random")
  expect_equal(unname(unlist(pm_tibble4$cols_rename)), c("value", "shrinkage"))
})


test_that("make_pmtable pmtable commands: notes", {
  pm_tibble5 <- make_pmtable(.df = newFormatDF, .pmtype = "fixed") %>% pmtables::st_notes(c("note 1", "note2"))
  pm_tibble6 <- make_pmtable(.df = newFormatDF, .pmtype = "fixed", .width = 2.1) %>% pmtables::st_notes(c("note 1", "note2"))

  expect_equal(pm_tibble5$notes, c("note 1", "note2"))
  expect_equal(pm_tibble5$note_config$width, 1)
  expect_equal(pm_tibble6$note_config$width, 2.1)
})

test_that("make_pmtable pmtable commands: args", {
  pm_tibble5 <- make_pmtable(.df = newFormatDF, .pmtype = "fixed")
  expect_equal(unlist(pm_tibble5$args), NULL)
})


test_that("make_pmtable correctly filters with .pmtype", {
  pm_tibble4 <- make_pmtable(.df = newFormatDF, .pmtype = "random")
  pm_tibble5 <- make_pmtable(.df = newFormatDF, .pmtype = "fixed")
  pm_tibble6 <- make_pmtable(.df = newFormatDF, .pmtype = "full")
  pm_tibble7 <- make_pmtable(.df = newFormatDF, .pmtype = "structural")

  #random
  expect_equal(
    newFormatDF %>%
      dplyr::filter(stringr::str_detect(greek, "Omega") |
                      stringr::str_detect(type, "Resid")) %>%
      nrow(),
    nrow(pm_tibble4$data)
  )

  #fixed
  expect_equal(
    newFormatDF %>%
      filter(
        stringr::str_detect(type, "Struct") |
          stringr::str_detect(type, "effect")
      ) %>%
      nrow(),
    nrow(pm_tibble5$data)
  )

  #full
  expect_equal(
    newFormatDF %>%
      nrow(),
    nrow(pm_tibble6$data)
  )

  #structural
  expect_equal(
    newFormatDF %>%
      dplyr::filter(stringr::str_detect(type, "Struct")
      ) %>%
      nrow(),
    nrow(pm_tibble7$data)
  )

})
