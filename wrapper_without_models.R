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
source("simulate_data.R")

# Stan model code strings
source("models.R")

# Diagnostic functions
source("diagnostics.R")

# Model fitting functions
source("fit_anything.R")

# Match models to their parameters
model_params <- list(
  one.two = c("spp_int","spp_slp"),
  one.three = c("spp_int","spp_slp",
                "mean_int","mean_slp",
                "lam_int","lam_slp",
                "null_int","null_slp"),
  two.one   = c("spp_int","spp_slp",
                "mean_int","mean_slp",
                "phy_int","phy_slp",
                "invphy_int","invphy_slp",
                "null_int","null_slp"),
  two.two   = c("null_sites","inv_sites"),
  three.one.base = c("spp_int","spp_slp",
                     "partner_int",
                     "mean_int","mean_slp",
                     "lam_int","lam_slp",
                     "null_int","null_slp",
                     "mean_partner_int",
                     "lam_partner_int",
                     "null_partner_int"),
  three.one.partner = c("spp_int","spp_slp",
                        "partner_int",
                        "mean_int","mean_slp",
                        "lam_int","lam_slp",
                        "null_int","null_slp",
                        "mean_partner_int",
                        "lam_partner_int",
                        "null_partner_int"))

#sim_data <- simulate_data(1)
#seed <- 1
# Begin loop
for(i in 1:101){
  tryCatch({
    
    # Simulate data
    sim_data <- simulate_data(i)
    seed <- i
    
    # Initialize results list
    results <- list()
    
    fit_file <- file.path(fits_dir, sprintf("all_models_seed_%d.rds", seed))
    if (!file.exists(fit_file))
      stop("Saved fit not found: ", fit_file)
    
    results <- readRDS(fit_file)
    
    # Gather parameter-level diagnostics
    param_results_df <- dplyr::bind_rows(
      # Q1
      get_param_diagnostics(results$one.two.mcmc, sim_data, "one.two.mcmc", model_params$one.two, truth = list(spp_int = sim_data$true$lab.occ, spp_slp = sim_data$true$cons.trait)),
      get_param_diagnostics(results$one.two.vb.mf, sim_data, "one.two.vb.mf", model_params$one.two, truth = list(spp_int = sim_data$true$lab.occ, spp_slp = sim_data$true$cons.trait)),
      get_param_diagnostics(results$one.two.vb.fr, sim_data, "one.two.vb.fr", model_params$one.two, truth = list(spp_int = sim_data$true$lab.occ, spp_slp = sim_data$true$cons.trait)),
      get_param_diagnostics(results$one.three.mcmc, sim_data, "one.three.mcmc", model_params$one.three, truth = list(spp_int = sim_data$true$lab.occ, spp_slp = sim_data$true$cons.trait)),
      get_param_diagnostics(results$one.three.vb.mf, sim_data, "one.three.vb.mf", model_params$one.three, truth = list(spp_int = sim_data$true$lab.occ, spp_slp = sim_data$true$cons.trait)),
      get_param_diagnostics(results$one.three.vb.fr, sim_data, "one.three.vb.fr", model_params$one.three, truth = list(spp_int = sim_data$true$lab.occ, spp_slp = sim_data$true$cons.trait)),
      
      # Q2
      get_param_diagnostics(results$two.one.mcmc, sim_data, "two.one.mcmc", model_params$two.one, truth = list(spp_int = sim_data$true$lab.occ, spp_slp = sim_data$true$comp.trait)),
      get_param_diagnostics(results$two.one.vb.mf, sim_data, "two.one.vb.mf", model_params$two.one, truth = list(spp_int = sim_data$true$lab.occ, spp_slp = sim_data$true$comp.trait)),
      get_param_diagnostics(results$two.one.vb.fr, sim_data, "two.one.vb.fr", model_params$two.one, truth = list(spp_int = sim_data$true$lab.occ, spp_slp = sim_data$true$comp.trait)),
      get_param_diagnostics(results$two.two.mcmc, sim_data, "two.two.mcmc", model_params$two.two),
      get_param_diagnostics(results$two.two.vb.mf, sim_data, "two.two.vb.mf", model_params$two.two),
      get_param_diagnostics(results$two.two.vb.fr, sim_data, "two.two.vb.fr", model_params$two.two),
      
      # Q3
      get_param_diagnostics(results$three.one.mcmc.base, sim_data, "three.one.mcmc.base", model_params$three.one.base, truth = list(spp_int = sim_data$true$base_occ, spp_slp = sim_data$true$base_env_trait, partner_int = sim_data$true$partner_assoc_trait)),
      get_param_diagnostics(results$three.one.vb.base.mf, sim_data, "three.one.vb.base.mf", model_params$three.one.base, truth = list(spp_int = sim_data$true$base_occ, spp_slp = sim_data$true$base_env_trait, partner_int = sim_data$true$partner_assoc_trait)),
      get_param_diagnostics(results$three.one.vb.base.fr, sim_data, "three.one.vb.base.fr", model_params$three.one.base, truth = list(spp_int = sim_data$true$base_occ, spp_slp = sim_data$true$base_env_trait, partner_int = sim_data$true$partner_assoc_trait)),
      get_param_diagnostics(results$three.one.mcmc.partner, sim_data, "three.one.mcmc.partner", model_params$three.one.partner, truth = list(partner_int = sim_data$true$base_assoc_trait)),
      get_param_diagnostics(results$three.one.vb.partner.mf, sim_data, "three.one.vb.partner.mf", model_params$three.one.partner, truth = list(partner_int = sim_data$true$base_assoc_trait)),
      get_param_diagnostics(results$three.one.vb.partner.fr, sim_data, "three.one.vb.partner.fr", model_params$three.one.partner, truth = list(partner_int = sim_data$true$base_assoc_trait))
    )
    
    # Gather model-level diagnostics
    model_results_df <- dplyr::bind_rows(
      # Q1
      get_model_diagnostics(results$one.two.mcmc, "one.two.mcmc"),
      get_model_diagnostics(results$one.two.vb.mf, "one.two.vb.mf"),
      get_model_diagnostics(results$one.two.vb.fr, "one.two.vb.fr"),
      get_model_diagnostics(results$one.three.mcmc, "one.three.mcmc"),
      get_model_diagnostics(results$one.three.vb.mf, "one.three.vb.mf"),
      get_model_diagnostics(results$one.three.vb.fr, "one.three.vb.fr"),
      
      # Q2
      get_model_diagnostics(results$two.one.mcmc, "two.one.mcmc"),
      get_model_diagnostics(results$two.one.vb.mf, "two.one.vb.mf"),
      get_model_diagnostics(results$two.one.vb.fr, "two.one.vb.fr"),
      get_model_diagnostics(results$two.two.mcmc, "two.two.mcmc"),
      get_model_diagnostics(results$two.two.vb.mf, "two.two.vb.mf"),
      get_model_diagnostics(results$two.two.vb.fr, "two.two.vb.fr"),
      
      # Q3
      get_model_diagnostics(results$three.one.mcmc.base, "three.one.mcmc.base"),
      get_model_diagnostics(results$three.one.vb.base.mf, "three.one.vb.base.mf"),
      get_model_diagnostics(results$three.one.vb.base.fr, "three.one.vb.base.fr"),
      get_model_diagnostics(results$three.one.mcmc.partner, "three.one.mcmc.partner"),
      get_model_diagnostics(results$three.one.vb.partner.mf, "three.one.vb.partner.mf"),
      get_model_diagnostics(results$three.one.vb.partner.fr, "three.one.vb.partner.fr")
    )
    
    # Save results
    write.csv(param_results_df, file=paste0("results/param_diagnostics_seed_",seed,".csv"), row.names=F)
    write.csv(model_results_df, file=paste0("results/model_diagnostics_seed_",seed,".csv"), row.names=F)
    
    # Free RAM before next loop
    rm(results); gc()
    
  }, error = function(e) {
    message("Iteration ", i, " failed: ", conditionMessage(e))})
}

source("plots/plot_RMSE.R")
