#defineRows

paramKey = dplyr::tribble(
  ~name, ~abb, ~desc, ~panel, ~trans,
  "THETA1",  "KA (1/h)", "First order absorption rate constant",   "struct", "logTrans",
  "THETA2", "V2/F (L)",  "Apparent central volume",                "struct", "logTrans",
  "THETA3", "CL/F (L/h)", "Apparent clearance",                    "struct", "logTrans",
  "THETA4", "V3/F (L)",  "Apparent peripheral volume",             "struct", "logTrans",
  "THETA5", "Q/F (L/h)", "Apparent intercompartmental clearance",  "struct", "logTrans",
  "THETA6", "$\\text{CL/F}_{eGFR}$", "eGFR effect on CL/F",        "cov",    "none",
  "THETA7", "$\\text{CL/F}_{AGE}$", "Age effect on CL/F",          "cov",    "none",
  "THETA8", "$\\text{CL/F}_{ALB}$", "Serum albumin effect on CL/F","cov",    "none",

  "OMEGA11", "IIV-KA",   "Variance of absorption",     "IIV", "lognormalOm",
  "OMEGA22", "IIV-V2/F", "Variance of central volume", "IIV", "lognormalOm",
  "OMEGA33", "IIV-CL/F", "Variance of clearance",      "IIV", "lognormalOm",

  "OMEGA21", "V2/F-KA", "Covariance of V2/F - KA",    "IIV", "none",
  "OMEGA31", "CL/F-KA", "Covariance of CL/F - KA",    "IIV", "none",
  "OMEGA32", "CL/F-V2/F", "Covariance of CL/F - V2/F","IIV", "none",

  "SIGMA11", "Proportional", "Variance", "RV", "propErr"
)


test_that("defineRows unexpected input: trans column is not in list of valid trans values [MPT-DRO-001]",{
  paramKey2 <- paramKey
  paramKey2$trans[1] <- "lgoTrans"
  expect_error(capture.output(defineRows(paramKey2)))
})

newDF <- defineRows(paramKey)

test_that("defineRows generates logical columns to indicate trans type [MPT-DRO-002]", {
  expect_equal(all(newDF$LOG[newDF$trans == "logTrans"]), TRUE)
  expect_equal(any(newDF$LOG[newDF$trans == "logitTrans"]), FALSE)
  expect_equal(all(newDF$LOGIT[newDF$trans == "logitTrans"]), TRUE)
  expect_equal(any(newDF$LOGIT[newDF$trans == "logTrans"]), FALSE)
})

test_that("defineRows generates logical columns to indicate name type [MPT-DRO-002]", {
  expect_equal(all(newDF$TH[newDF$name == "THETA1"]), TRUE)
  expect_equal(all(newDF$TH[newDF$name == "SIGMA11"]), FALSE)
  expect_equal(all(newDF$S[newDF$name == "SIGMA11"]), TRUE)
  expect_equal(all(newDF$S[newDF$name == "THETA1"]), FALSE)
})


