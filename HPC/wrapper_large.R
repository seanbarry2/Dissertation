# This wrapper basically explores how the computation time grows when
# species/sites are increased for model 2.2

# Good practice
rm(list = ls())

# Make results folders
if(!dir.exists("results")) dir.create("results")
# Folder for saving models themselves
fits_dir <- file.path("results", "fits")
if (!dir.exists(fits_dir)) dir.create(fits_dir, recursive = TRUE)

# Load or install all necessary packages
source("setup.R")

# Simulates data
source("simulate_data_large.R")

# Stan model code strings
source("models.R")

# Diagnostic functions
source("diagnostics.R")

# Model fitting functions
source("fit_anything.R")

# Parallelize
rstan_options(auto_write = TRUE)
options(mc.cores = 6)

# Compile Stan models once globally
sm_two_two <- stan_model(model_code = two.two.stan.code)

# Match models to their parameters
model_params <- list(two.two = c("null_sites","inv_sites"))

# Begin loop
for(i in 15:30){
  tryCatch({
    
    # Simulate data
    sim_data <- simulate_data(seed = sample.int(10000, 1), species = i, sites = i)
    seed <- sample.int(10000, 1)
    
    # Initialize results list
    results <- list()
    
    # Q2
    results$two.two.mcmc <- fit_any_mcmc_timed(two.two.stan.code, sim_data$two_two_data, seed = seed, iter = 2000, chains = 4)
    results$two.two.vb.mf <- fit_any_vb_timed(sm_two_two, sim_data$two_two_data, seed = seed, iter = 20000, tol = 0.0001, algo = "meanfield")
    results$two.two.vb.fr <- fit_any_vb_timed(sm_two_two, sim_data$two_two_data, seed = seed, iter = 20000, tol = 0.0001, algo = "fullrank")

    # Save model fits (optional)
    # saveRDS(results, file = file.path(fits_dir, sprintf("two_two_models_seed_%d.rds", seed)), compress = "xz")
    
    # Gather parameter-level diagnostics
    param_results_df <- dplyr::bind_rows(
      # Q2
      get_param_diagnostics(results$two.two.mcmc, sim_data, "two.two.mcmc", model_params$two.two),
      get_param_diagnostics(results$two.two.vb.mf, sim_data, "two.two.vb.mf", model_params$two.two),
      get_param_diagnostics(results$two.two.vb.fr, sim_data, "two.two.vb.fr", model_params$two.two),
    )
    
    # Gather model-level diagnostics
    model_results_df <- dplyr::bind_rows(
      # Q2
      get_model_diagnostics(results$two.two.mcmc, "two.two.mcmc"),
      get_model_diagnostics(results$two.two.vb.mf, "two.two.vb.mf"),
      get_model_diagnostics(results$two.two.vb.fr, "two.two.vb.fr"),
    )
    
    # Save results
    write.csv(param_results_df, file=paste0("results/question_two_param_diagnostics_seed_",seed,".csv"), row.names=F)
    write.csv(model_results_df, file=paste0("results/question_two_model_diagnostics_seed_",seed,".csv"), row.names=F)
    
    # Free RAM before next loop
    rm(results); gc()
    
  }, error = function(e) {
    message("Iteration ", i, " failed: ", conditionMessage(e))})
}
