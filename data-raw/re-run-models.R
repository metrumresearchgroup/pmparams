devtools::load_all()
library(bbr)

model_dir <- "inst/model/nonmem"
model_dir <- file.path(rprojroot::find_rstudio_root_file(), model_dir)

mod2 <- read_model(file.path(model_dir, "102"))
mod6 <- read_model(file.path(model_dir, "106"))

message("Submitting mod102 and mod106 locally")
submit_models(list(mod2, mod6), .mode = "local", .overwrite = TRUE)


message("Saving out parameter estimates for mod102 and mod106")
param_est_102 <- model_summary(mod2) %>% param_estimates()
param_est_106 <- model_summary(mod6) %>% param_estimates()

readr::write_csv(param_est_102, file.path(model_dir, "param_est_102.csv"))
readr::write_csv(param_est_106, file.path(model_dir, "param_est_106.csv"))


# Bootstrap
boot_run <- new_bootstrap_run(mod6, .overwrite = TRUE)
setup_bootstrap_run(boot_run, strat_cols = "STUDY", n = 200, .overwrite = TRUE)

message("Submitting bootstrap of mod106 on the grid")
submit_model(boot_run, .overwrite = TRUE)

# Wait for bootstrap estimates to finish on the grid
# get_model_status(boot_run)

if(check_nonmem_finished(boot_run)){
  summarize_bootstrap_run(boot_run)
  cleanup_bootstrap_run(boot_run, .force = TRUE)
}
