# parens

test_that("parens wraps string in parentheses", {
  expect_equal(parens("Hello"), "(Hello)")
  expect_equal(parens(14), "(14)")
})

test_that("parens wraps vector values in parentheses", {
  expect_equal(parens(c("Dose 14", "Dose 15", "Dose 16")), c("(Dose 14)", "(Dose 15)", "(Dose 16)"))
  expect_equal(parens(c(14, 15, 16)), c("(14)", "(15)", "(16)"))
})

# parensSQ

test_that("parensSQ wraps string in brackets", {
  expect_equal(parensSQ("Hello"), "[Hello]")
  expect_equal(parensSQ(14), "[14]")
})

test_that("parens wraps vector values in brackets", {
  expect_equal(parensSQ(c("Dose 14", "Dose 15", "Dose 16")), c("[Dose 14]", "[Dose 15]", "[Dose 16]"))
  expect_equal(parensSQ(c(14, 15, 16)), c("[14]", "[15]", "[16]"))
})

# parensSQ_corr

test_that("parensSQ_corr wraps numeric in brackets with corr= prefix", {
  expect_equal(parensSQ_corr(1), "[Corr=1]")
})

# parensSQ_se

test_that("parensSQ_se wraps numeric in brackets with SD= prefix", {
  expect_equal(parensSQ_se(1), "[SD=1]")
})
