#check transforms
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

param_df <- bbr::read_model(system.file("model/nonmem/102", package = "mrgparamtab")) %>%
  bbr::model_summary() %>%
  bbr::param_estimates() %>%
  dplyr::mutate(name = gsub("[[:punct:]]", "", parameter_names)) %>%
  dplyr::inner_join(paramKey, by = "name")

test_that("checkTranforms unexpected input: no trans column [MPT-CTF-001]", {
  expect_error(checkTransforms(param_df %>% select(-trans)))
})

test_that("checkTransforms works with trans column with no tildes [MPT-CTF-002]", {
  expect_equal(param_df %>% dplyr::mutate(transTHETA = NA), checkTransforms(param_df))
})

test_that("checkTransforms works with trans column with tilde [MPT-CTF-002]", {
  param_df2 <- param_df
  param_df2$trans[1] <- "logTrans~lognormalOm"
  df1 <- checkTransforms(param_df2)
  expect_equal(df1$trans[1], "logTrans")
})


