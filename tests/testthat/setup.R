library(dplyr)
library(testthat)
library(mrgparamtab)

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

param_path <- system.file("model/nonmem/102", package = "mrgparamtab")

# param_est <- bbr::read_model(system.file("model/nonmem/102", package = "mrgparamtab")) %>%
#              bbr::model_summary() %>%
#              bbr::param_estimates()
# write.csv(param_est, here::here("inst", "model", "nonmem", "param_est.csv"))
param_est <- read.csv(system.file("model/nonmem/param_est.csv", package = "mrgparamtab"))
boot_param_est <- read.csv(system.file("model/nonmem/boot/data/boot-106.csv", package = "mrgparamtab"))

param_model <- bbr::read_model(system.file("model/nonmem/102", package = "mrgparamtab"))

newDF <- defineParamTable(.estimates = param_est, .key = paramKey, .ci = 95, .zed = NULL)

newbootDF <- defineBootTable(.boot_estimates =boot_param_est, .key = paramKey, .ci = 95, .zed = NULL)
#' #boot generate
#' #' Add `boot_dir` and create appropriate directories.
#' pk_model_dir <- "inst/model/nonmem"
#' boot_dir <- file.path(pk_model_dir,"boot")
#' boot_data_dir <- file.path(boot_dir, "data")
#'
#' #' Create template from original `.ctl` file, removing `COV`, `tables`, `MSF`
#' template_ctl <- readLines(file.path(pk_model_dir, "boot-102-template.ctl"))
#'
#' #' In the template:
#' #' - $DATA ../data/{{run_num}}.csv
#' #' - Optionally add OID at the end of $INPUT; but not required
#'
#' nmdata <- read.csv("../../data/derived/analysis3.csv", na = '.')
#' nmdata <- filter(nmdata, is.na(C))
#'
#' #' ## Looped over multiple times
#' RUN <- seq(100)
#'
#' make_boot_run <- function(n, data, template, boot_dir, strat_cols = NULL,
#'                           overwrite = FALSE,  seed = 21181) {
#'
#'   if (n %% 100 == 0) message(glue("Created {n} bootstrap data sets"))
#'   mod_name <- pad_left(n, 3)
#'
#'   mod_path <- glue("{boot_dir}/{mod_name}")
#'
#'   if(file.exists(paste0(mod_path, ".yaml")) && !overwrite) {
#'     return(read_model(mod_path))
#'   }
#'
#'   data_new <- resample_df(
#'     data,
#'     key_cols = "ID",
#'     strat_cols = strat_cols
#'   )
#'
#'   data_new <- rename(data_new, OID = ID, ID = KEY)
#'
#'   data_new <- select(data_new, unique(c("C", names(data), "OID")))
#'
#'   csv_file <- glue("{boot_dir}/data/{mod_name}.csv")
#'
#'   fwrite(data_new, csv_file , na = '.', quote = FALSE)
#'
#'   new_ctl <- whisker.render(template, list(run_num = mod_name))
#'
#'   write_file(new_ctl, file = paste0(mod_path, ".ctl"))
#'
#'   mod <- new_model(
#'     mod_path,
#'     .description = glue("bootstrap {mod_name}"),
#'     .overwrite = overwrite
#'   )
#'
#'   mod
#' }
#'
#'
#' #' For each proposed bootstrap run
#' #' - Create a bootstrap version of the data set
#' #'   - Stratify on `RF` and `CP`
#' #'   - Write out to csv file
#' #' - Create a bootstrap version of the ctl file
#' #'   - Update with bootstrap data set
#' #' - Create a model object
#'
#' set.seed(12345)
#'
#' models <- purrr::map(
#'   RUN,
#'   data = nmdata,
#'   .f = make_boot_run,
#'   template = template_ctl,
#'   boot_dir = boot_dir,
#'   strat_cols = c("RF", "CP"),
#'   overwrite = TRUE
#' )
#'
#' #' # Submit models to run
#'
#' #' This is where you actually run the models. If the output directories already
#' #' exist then these have already been run and you don't need to run them again.
#'
#' #' Submit first model locally to check that it runs fine.  This code is for
#' #' example only and is commented out.
#'
#' # models[[1]] %>% submit_model(.mode = "local", .bbi_args=list(overwrite=TRUE))
#'
#' out <- submit_models(
#'   models,
#'   .config_path = "../model/pk/bbi.yaml",
#'   .bbi_args = list(overwrite = TRUE, threads = 1)
#' )
#'
#'
#' #boot collect
#'
#'
#' boot_dir <- here::here("inst", "model", "nonmem", "boot")
#'
#' template_ctl <- readLines(
#'   file.path(pk_model_dir, "boot-106-template.ctl")
#' )
#' mod_id <- "106" # model name of original model
#'
#' boot <- bbr::param_estimates_batch(boot_dir)
#'
#' # Check for runs with [bbr/bbi] error messages
#' # Errors mean [bbi] failed to parse model outputs
#' # If there are any, you should check those models
#' #   manually to see what happened
#' err <- filter(boot, !is.na(error_msg))
#' nrow(err)
#'
#' # Do a quick comparison of the original estimates and the new estimates
#' # you can do `summary(boot)`, but the code below makes a nice tibble to look at
#' orig_mod <- read_model(here("model", "pk", mod_id))
#' param_estimates_compare(
#'   boot,
#'   orig_mod
#' )
#'
#' # Write bootstrap parameter estimates to file
#' boot %>%
#'   select(run, starts_with(c("THETA", "OMEGA", "SIGMA"))) %>%
#'   write_csv(here("data", "boot", glue("boot-{mod_id}.csv")))
