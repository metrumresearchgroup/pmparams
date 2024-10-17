library(dplyr)
library(bbr)
library(testthat)
library(pmparams)

paramKey <- dplyr::tribble(
  ~name, ~abb, ~desc, ~panel, ~trans,
  "THETA1",  "KA (1/h)", "First order absorption rate constant",   "struct", "logTrans",
  "THETA2", "V2/F (L)",  "Apparent central volume",                "struct", "logTrans",
  "THETA3", "CL/F (L/h)", "Apparent clearance",                    "struct", "logTrans",
  "THETA4", "V3/F (L)",  "Apparent peripheral volume",             "struct", "logTrans",
  "THETA5", "Q/F (L/h)", "Apparent intercompartmental clearance",  "struct", "logTrans",
  "THETA6", "CL/F ~ eGFR", "eGFR effect on CL/F",        "cov",    "none",
  "THETA7", "CL/F ~ AGE", "Age effect on CL/F",          "cov",    "none",
  "THETA8", "CL/F ~ ALB", "Serum albumin effect on CL/F","cov",    "none",

  "OMEGA11", "IIV-KA",   "Variance of absorption",     "IIV", "lognormalOm",
  "OMEGA22", "IIV-V2/F", "Variance of central volume", "IIV", "lognormalOm",
  "OMEGA33", "IIV-CL/F", "Variance of clearance",      "IIV", "lognormalOm",

  "OMEGA21", "V2/F-KA", "Covariance of V2/F - KA",    "IIV", "none",
  "OMEGA31", "CL/F-KA", "Covariance of CL/F - KA",    "IIV", "none",
  "OMEGA32", "CL/F-V2/F", "Covariance of CL/F - V2/F","IIV", "none",

  "SIGMA11", "Proportional", "Variance", "RV", "propErr"
)

model_dir <- system.file("model/nonmem", package = "pmparams")

paramKey_path <-  file.path(model_dir, "pk-parameter-key-new.yaml")
paramKeyBoth_path <-  file.path(model_dir, "pk-parameter-key-both.yaml")
param_yaml <- yaml::yaml.load_file(paramKey_path)

# Data for testing param table (no bootstrap)
paramPath <- file.path(model_dir, "102")
paramEst <- readr::read_csv(file.path(model_dir, "param_est.csv"))
paramModel <- bbr::read_model(paramPath)

newDF <- define_param_table(.estimates = paramEst, .key = paramKey, .ci = 95, .zscore = NULL)
newFormatDF <- format_param_table(newDF)
newFormatDFprse  <- format_param_table(newDF, .prse = T)

# Data for testing bootstrap param table
boot_paramEstPath <- file.path(model_dir, "boot/data/boot-106.csv")
boot_paramEst <- readr::read_csv(boot_paramEstPath)

nonboot_paramEst <- readr::read_csv(file.path(model_dir, "nonboot_param_est.csv"))

newbootDF <- pmparams::define_boot_table(.boot_estimates =boot_paramEst, .key = paramKey)
formatBootDF <- pmparams::format_boot_table(.boot_df = newbootDF, .cleanup_cols =  T)

newbootDF2 <- pmparams::define_boot_table(
  .boot_estimates = boot_paramEst,
  .key = paramKey,
  .percentiles  = c(0.29, 0.15, 0.99)
)

# final output
nonbootDF <- pmparams::define_param_table(.estimates = nonboot_paramEst, .key = paramKey)
formatnonbootDF <- nonbootDF %>% pmparams::format_param_table()


bootParam <-  left_join(formatnonbootDF, formatBootDF, by = c("abb", "desc"))

# testing theta error block
theta_err <- paramEst %>%  mutate(
  parameter_names = if_else(parameter_names == "SIGMA(1,1)", "THETA(1,1)", parameter_names)
)
theta_err_key = paramKey %>% mutate(name = if_else(name == "SIGMA11", "THETA11", name))
theta_err_df1 <- define_param_table(.estimates = theta_err, .key = theta_err_key, .ci = 95, .zscore = NULL)

# bbr objects
MOD <- bbr::read_model(file.path(model_dir, "106"))
BOOT_RUN <- bbr::read_model(file.path(model_dir, "106-boot"))
