


test_that("display_value works", {

  cases <- list(
    list(
      input = list(x = -4.2, .digit = NULL, .maxex = NULL),
      want = "$-$4.20"
    ),
    list(
      input = list(x = c(-5.464, -2.362), .digit = NULL, .maxex = NULL),
      want = c("$-$5.46", "$-$2.36")
    ),
    list(
      input = list(x = -2:2, .digit = NULL, .maxex = NULL),
      want = c("$-$2", "$-$1", "0", "1", "2")
    ),
    list(
      input = list(x = -3431.35, .digit = 5, .maxex = 4),
      want = "$-$3431.4"
    ),
    list(
      input = list(x = -3431.35, .digit = NULL, .maxex = 3),
      want = "$-$3.43e+03"
    )
  )

  for(case in cases){
    expect_identical(
      display_value(case$input$x, case$input$.digit, case$input$.maxex),
      case$want
    )
  }
})



