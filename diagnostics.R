# -------------------------------------------------------------------
# Diagnostic functions
# -------------------------------------------------------------------

# 1. Credible intervals --------------------------------------------------------
posterior_ci <- function(fit, par_name, probs = c(0.025, 0.975)) {
  draws <- rstan::extract(fit, pars = par_name)[[1]]
  
  if (length(dim(draws)) <= 1 || dim(draws)[2] == 1) {
    return(stats::quantile(as.vector(draws), probs = probs))
  } else {
    apply(draws, 2, stats::quantile, probs = probs)
  }
}

# 2. CI Width ------------------------------------------------------------------
ci_width <- function(ci_mat) ci_mat[2, ] - ci_mat[1, ]   # 2 × K → length-K

# 3. Point estimates -----------------------------------------------------------
posterior_point  <- function(fit, par_name, fun = mean) {
  draws <- rstan::extract(fit, pars = par_name)[[1]]
  if (length(dim(draws)) <= 1) {
    return(fun(draws))
  }
  as.vector(apply(draws, 2, fun))
}
posterior_scalar <- function(fit, par_name, fun = mean)
  fun(rstan::extract(fit, pars = par_name)[[1]])

# 4. Coverage (true value inside 95 % CI?) -------------------------------------
coverage <- function(fit,              # stanfit object
                     par_name,         # parameter name (scalar or vector)
                     truth,            # numeric scalar or vector of true values
                     probs = c(0.025, 0.975)) {
  
  ci <- posterior_ci(fit, par_name, probs)   # 2 × K matrix or length-2 vector
  
  if (is.matrix(ci)) {
    # align names if present
    if (!is.null(colnames(ci)) && !is.null(names(truth)))
      truth <- truth[colnames(ci)]
    
    inside <- (truth > ci[1, ]) & (truth < ci[2, ])
  } else {
    inside <- (truth > ci[1]) & (truth < ci[2])
  }
  
  list(
    inside = inside,
    prop   = mean(inside)
  )
}

# 5. Pareto-k ------------------------------------------------------------------
pareto_k <- function(fit, merge_chains = FALSE) {
  ll  <- loo::extract_log_lik(fit, merge_chains = merge_chains)
  loo <- loo::loo(ll, moment_match = FALSE)
  loo::pareto_k_values(loo)
}

# -------------------------------------------------------------------
# Helpers that summarise whole-model / per-parameter diagnostics
# -------------------------------------------------------------------

get_model_diagnostics <- function(result, model_name) {
  
  elapsed_time <- attr(result, "elapsed_time")
  if (is.null(elapsed_time)) elapsed_time <- NA_real_
  
  if (is.null(result)) {
    return(
      data.frame(model            = model_name,
                 max_k            = NA_real_,
                 prop_bad_k       = NA_real_,
                 elapsed_time_sec = elapsed_time,
                 stringsAsFactors = FALSE)
    )
  }
  
  k_vec <- pareto_k(result)
  
  data.frame(model            = model_name,
             max_k            = max(k_vec),
             prop_bad_k       = mean(k_vec > 0.7),
             elapsed_time_sec = elapsed_time,
             stringsAsFactors = FALSE)
}

# -------------------------------------------------------------------
# Parameter-level diagnostics
# -------------------------------------------------------------------

get_param_diagnostics <- function(result, sim_data, model_name, pars,
                                  truth = NULL, probs = c(0.025, 0.975)) {
  
  # Handle failed / NULL fits ---------------------------------------------------
  if (inherits(result, "error") || is.null(result)) {
    return(
      data.frame(
        model          = model_name,
        parameter      = pars,
        ci_lower       = NA_real_,
        ci_upper       = NA_real_,
        ci_width_mean  = NA_real_,
        point_estimate = NA_real_,
        RMSE           = NA_real_,
        CI_True        = NA_integer_,
        CI_False       = NA_integer_
      )
    )
  }
  
  scalar_pars <- c("lam_slp", "lam_int", "null_slp", "null_int", "mean_slp",
                   "mean_int", "phy_slp", "phy_int", "invphy_slp", "invphy_int",
                   "null_sites", "inv_sites", "lam_partner_int",
                   "null_partner_int", "mean_partner_int")  # formerly "partner_int"
  
  dplyr::bind_rows(
    lapply(pars, function(par) {
      
      # --- Posterior CI --------------------------------------------------------
      ci <- posterior_ci(result, par, probs = probs)
      
      # Match truth for vector parameters --------------------------------------
      truth_par <- if (is.list(truth)) truth[[par]] else truth
      
      if (is.null(ci)) {
        ci_lower      <- NA_real_
        ci_upper      <- NA_real_
        ci_width_mean <- NA_real_
      } else {
        ci_lower      <- ci[1]
        ci_upper      <- ci[2]
        ci_width_mean <- if (length(ci) == 2) diff(ci) else mean(ci_width(ci))
      }
      
      # --- Point estimate ------------------------------------------------------
      pe <- if (par %in% scalar_pars) {
        posterior_scalar(result, par)
      } else {
        mean(posterior_point(result, par))
      }
      
      post_vec <- if (par %in% scalar_pars) {
        posterior_scalar(result, par)          # length-1
      } else {
        posterior_point(result, par)           # length K
      }
      
      # --- RMSE ----------------------------------------------------------------
      if (!is.null(truth_par) && length(post_vec) == length(truth_par)) {
        rmse <- sqrt(mean((unname(post_vec) - unname(truth_par))^2))
      } else {
        rmse <- NA_real_
      }
      
      # --- Coverage counts -----------------------------------------------------
      if (!is.null(truth_par) && length(post_vec) == length(truth_par)) {
        inside_vec <- coverage(result, par, truth_par, probs = probs)$inside
        CI_True    <- sum(inside_vec,  na.rm = TRUE)
        CI_False   <- sum(!inside_vec, na.rm = TRUE)
      } else {
        CI_True  <- NA_integer_
        CI_False <- NA_integer_
      }
      
      # --- Row for this parameter ---------------------------------------------
      data.frame(
        model          = model_name,
        parameter      = par,
        ci_lower       = ci_lower,
        ci_upper       = ci_upper,
        ci_width_mean  = ci_width_mean,
        point_estimate = pe,
        RMSE           = rmse,
        CI_True        = CI_True,
        CI_False       = CI_False
      )
    })
  )
}

# -------------------------------------------------------------------
# Save a fitted model object
# -------------------------------------------------------------------

save_fit <- function(fit_object, file_stub, seed, dir = fits_dir) {
  if (!inherits(fit_object, "error") && !is.null(fit_object)) {
    fn <- file.path(dir, sprintf("%s_seed_%d.rds", file_stub, seed))
    saveRDS(fit_object, file = fn, compress = "xz")
  }
}
