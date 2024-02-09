library(dplyr)
library(testthat)
library(pmparams)

skip_if_no_bbi <- function(.test_name) {
  # if bbi_version() can't find bbi, it returns ""
  if (!nzchar(bbr::bbi_version())) {
    testthat::skip(paste(.test_name, "needs bbi installed"))
  }
}

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

#Data for testing param table (no boot)

paramPath <- system.file("model/nonmem/102", package = "pmparams")
paramEst <- utils::read.csv(system.file("model/nonmem/param_est.csv", package = "pmparams"))
paramModel <- bbr::read_model(system.file("model/nonmem/102", package = "pmparams"))

newDF <- define_param_table(.estimates = paramEst, .key = paramKey, .ci = 95, .zscore = NULL)

#Data for testing boot param table
boot_paramEstPath <- system.file("model/nonmem/boot/data/boot-106.csv", package = "pmparams")
boot_paramEst <- utils::read.csv(system.file("model/nonmem/boot/data/boot-106.csv", package = "pmparams"))

nonboot_paramEstPath <- system.file("model/nonmem/106", package = "pmparams")
nonboot_paramEst <- utils::read.csv(system.file("model/nonmem/nonboot_param_est.csv", package = "pmparams"))

newbootDF <- pmparams::define_boot_table(.boot_estimates =boot_paramEst, .nonboot_estimates = nonboot_paramEst, .key = paramKey)
formatBootDF <- pmparams::format_boot_table(.boot_df = newbootDF)

#final output
nonbootDF <- pmparams::define_param_table(.estimates = nonboot_paramEst, .key = paramKey)
formatnonbootDF <- nonbootDF %>% pmparams::format_param_table()


bootParam <-  left_join(formatnonbootDF, formatBootDF, by = c("abb", "desc"))

#testing theta error block
theta_err = paramEst %>% as.data.frame() %>% mutate(parameter_names = if_else(parameter_names == "SIGMA(1,1)", "THETA(1,1)", parameter_names))
theta_err_key = paramKey %>% mutate(name = if_else(name == "SIGMA11", "THETA11", name))
theta_err_df1 <- define_param_table(.estimates = theta_err, .key = theta_err_key, .ci = 95, .zscore = NULL)
