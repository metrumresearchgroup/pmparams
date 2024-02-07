
test_that("make_pmtable incorrect input type: invalid .pmtype",{
  expect_error(capture.output(make_pmtable(.df = newFormatDF, .pmtype = "no type")))
})
