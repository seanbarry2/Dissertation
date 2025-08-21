# Script that contains functions that fit VB models and MCMC models

# Function that allows you to fit any MCMC model
fit_any_mcmc <- function(model_code, sm_data, seed, iter = 1000, chains = 4) {
  stan(model_code = model_code, data = sm_data, iter = iter, chains = chains, seed = seed)
}

# Function that allows you to fit any VB model
fit_any_vb <- function(sm, sm_data, seed, iter = 20000, tol = 0.0001, algo = "meanfield", init = "random", eta = NULL) {
  vb(sm, sm_data, seed = seed, iter = iter, tol_rel_obj = tol, algorithm = algo, init = init, eta = eta)
}

# Wrapper that keep track of time for MCMC and has error handling
fit_any_mcmc_timed <- function(...) {
  start <- Sys.time()
  fit <- tryCatch(fit_any_mcmc(...), error = function(e) e)
  attr(fit, "elapsed_time") <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  fit
}

# Wrapper that keep track of time for VB and has error handling
fit_any_vb_timed <- function(...) {
  start <- Sys.time()
  fit <- tryCatch(fit_any_vb(...), error = function(e) { message("VB FAILED: ", e$message)
    NULL })
  elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  
  if (!is.null(fit)) {
    attr(fit, "elapsed_time") <- elapsed
  } else {
    message("VB model returned NULL after failure.")
  }
  fit
}
