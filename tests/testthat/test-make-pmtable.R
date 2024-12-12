
test_that("make_pmtable pmtable commands: st_panel", {
  pm_tibble1 <- make_pmtable(.df = FMT_PARAM_TAB_102, .pmtype = "full")
  expect_equal(unname(unlist(pm_tibble1$panel$col)), "type")
})

test_that("make_pmtable pmtable commands: make cols blank", {
  pm_tibble1 <- make_pmtable(.df = FMT_PARAM_TAB_102, .pmtype = "full")
  expect_equal(unname(unlist(pm_tibble1$cols_blank)), c("abb", "greek", "desc"))

  pm_tibble2 <- make_pmtable(.df = FMT_PARAM_TAB_102, .pmtype = "random")
  expect_equal(unname(unlist(pm_tibble2$cols_blank)), c("abb", "greek"))
})

test_that("make_pmtable pmtable commands: rename cols", {
  pm_tibble3 <- make_pmtable(.df = FMT_PARAM_TAB_102, .pmtype = "structural")
  expect_equal(unname(unlist(pm_tibble3$cols_rename[1])), "value")

  pm_tibble4 <- make_pmtable(.df = FMT_PARAM_TAB_102, .pmtype = "random")
  expect_equal(unname(unlist(pm_tibble4$cols_rename)), c("value", "shrinkage", "pRSE"))
})


test_that("make_pmtable pmtable commands: notes", {
  pm_tibble5 <- make_pmtable(.df = FMT_PARAM_TAB_102, .pmtype = "fixed") %>% pmtables::st_notes(c("note 1", "note2"))
  pm_tibble6 <- make_pmtable(.df = FMT_PARAM_TAB_102, .pmtype = "fixed", .width = 2.1) %>% pmtables::st_notes(c("note 1", "note2"))

  expect_equal(pm_tibble5$notes, c("note 1", "note2"))
  expect_equal(pm_tibble5$note_config$width, 1)
  expect_equal(pm_tibble6$note_config$width, 2.1)
})

test_that("make_pmtable pmtable commands: args", {
  pm_tibble5 <- make_pmtable(.df = FMT_PARAM_TAB_102, .pmtype = "fixed")
  expect_equal(unlist(pm_tibble5$args), NULL)
})


test_that("make_pmtable correctly filters with .pmtype", {
  pm_tibble4 <- make_pmtable(.df = FMT_PARAM_TAB_102, .pmtype = "random")
  pm_tibble5 <- make_pmtable(.df = FMT_PARAM_TAB_102, .pmtype = "fixed")
  pm_tibble6 <- make_pmtable(.df = FMT_PARAM_TAB_102, .pmtype = "full")
  pm_tibble7 <- make_pmtable(.df = FMT_PARAM_TAB_102, .pmtype = "structural")

  #random
  expect_equal(
    7,
    nrow(pm_tibble4$data)
  )

  #fixed
  expect_equal(
    5,
    nrow(pm_tibble5$data)
  )

  #full
  expect_equal(
    12,
    nrow(pm_tibble6$data)
  )

  #structural
  expect_equal(
    5,
    nrow(pm_tibble7$data)
  )

})

test_that("make_pmtable pmtable commands: includes prse", {
  pm_tibble8 <- make_pmtable(.df = FMT_PARAM_TAB_102_PRSE, .pmtype = "full")
  st_pm_tibble8 <- pm_tibble8 %>% pmtables::stable()

  expect_true("pRSE" %in% names(pm_tibble8))
  expect_true(any(grepl("RSE", st_pm_tibble8)))


  pm_tibble9 <- make_pmtable(.df = FMT_PARAM_TAB_102_PRSE, .pmtype = "structural")
  st_pm_tibble9 <- pm_tibble9 %>% pmtables::stable()

  expect_true("pRSE" %in% names(pm_tibble9))
  expect_true(any(grepl("RSE", st_pm_tibble9)))
})
