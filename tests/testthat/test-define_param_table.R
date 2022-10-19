param_est <- bbr::read_model(system.file("model/nonmem/102", package = "mrgparamtab")) %>%
  bbr::model_summary() %>%
  bbr::param_estimates()

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

newDF <- define_param_table(.estimates = param_est, .key = paramKey)

test_that("define_param_table creates new parameter names without parentheses", {
  expect_equal(newDF$name[newDF$parameter_names == "OMEGA(1,1)"], "OMEGA11")
})

test_that("define_param_table generates logical columns to indicate parameter type", {
  expect_equal(newDF$TH[newDF$parameter_names == "THETA1"], TRUE)
  expect_equal(newDF$OM[newDF$parameter_names == "THETA1"], FALSE)
  expect_equal(newDF$OM[newDF$parameter_names == "OMEGA(1,1)"], TRUE)
  expect_equal(newDF$S[newDF$parameter_names == "SIGMA(1,1)"], TRUE)
  expect_equal(newDF$S[newDF$parameter_names == "THETA1"], FALSE)
})

test_that("define_param_table accurately generates logical columns for transformation", {
  expect_true(newDF$trans[newDF$name == "THETA1"] == "logTrans" &
                newDF$LOG[newDF$name == "THETA1"] == TRUE &
                newDF$LOGIT[newDF$name == "THETA1"] == FALSE)

  expect_true(newDF$trans[newDF$name == "OMEGA11"] == "lognormalOm" &
                newDF$LOG[newDF$name == "OMEGA11"] == FALSE &
                newDF$lognormO[newDF$name == "OMEGA11"] == TRUE)

  expect_true(newDF$trans[newDF$name == "SIGMA11"] == "propErr" &
                newDF$LOG[newDF$name == "SIGMA11"] == FALSE &
                newDF$propErr[newDF$name == "SIGMA11"] == TRUE)
})
