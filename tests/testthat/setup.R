library(dplyr)
library(testthat)
library(pmparams)

PARAM_KEY_DF = dplyr::tribble(
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


MODEL_DIR <- system.file("model/nonmem", package = "pmparams")

PARAM_KEY_PATH <-  file.path(MODEL_DIR, "pk-parameter-key-new.yaml")
PARAM_KEY_YAML <- yaml::yaml.load_file(PARAM_KEY_PATH)
PARAM_KEY_PATH_DF <-  file.path(MODEL_DIR, "pk-parameter-key.yaml")
PARAM_KEY_PATH_BOTH <-  file.path(MODEL_DIR, "pk-parameter-key-both.yaml")


# Data for testing param table (no bootstrap)
MOD102_PATH <- file.path(MODEL_DIR, "102")
PARAM_EST_102 <- readr::read_csv(file.path(MODEL_DIR, "param_est_102.csv"), show_col_types = FALSE)

PARAM_TAB_102 <- define_param_table(
  .estimates = PARAM_EST_102,
  .key = PARAM_KEY_DF,
  .ci = 95, .zscore = NULL
)
FMT_PARAM_TAB_102 <- format_param_table(PARAM_TAB_102)
FMT_PARAM_TAB_102_PRSE  <- format_param_table(PARAM_TAB_102, .prse = TRUE)

# Data for testing boot param table
MOD106_PATH <- file.path(MODEL_DIR, "106")
PARAM_EST_106 <- readr::read_csv(file.path(MODEL_DIR, "param_est_106.csv"), show_col_types = FALSE)

BOOT_106_EST_PATH <- file.path(MODEL_DIR, "boot", "data", "boot-106.csv")
BOOT_106_EST <- readr::read_csv(BOOT_106_EST_PATH, show_col_types = FALSE)

BOOT_TAB_106 <- pmparams::define_boot_table(
  .boot_estimates = BOOT_106_EST,
  .nonboot_estimates = PARAM_EST_106,
  .key = PARAM_KEY_DF
)
FMT_BOOT_TAB_106 <- pmparams::format_boot_table(.boot_df = BOOT_TAB_106)

# testing theta error block
THETA_ERR = PARAM_EST_102 %>% as.data.frame() %>% mutate(parameter_names = if_else(parameter_names == "SIGMA(1,1)", "THETA(1,1)", parameter_names))
THETA_ERR_KEY = PARAM_KEY_DF %>% mutate(name = if_else(name == "SIGMA11", "THETA11", name))
THETA_ERR_PARAM_TAB <- define_param_table(
  .estimates = THETA_ERR,
  .key = THETA_ERR_KEY,
  .ci = 95, .zscore = NULL
) %>% suppressMessages()
