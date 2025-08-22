**Overview**

This repository contains a modular re-implementation of the Gallinat & Pearse (2021) PGLMM tutorial. It simulates community data, fits a suite of Bayesian models in Stan via rstan using MCMC and Variational Bayes (VB; mean-field & full-rank), and writes out model- and parameter-level diagnostics for comparison (RMSE, 95% credible intervals, Pareto-k, runtime).

**Requirements**

R version 4 or newer, including packages: "dplyr", "geiger", "mvtnorm", "pez", "lme4", "rstan" and "loo" (can be downloaded by sourcing setup.R). The largest amount of cores used at once is 4, which represents the minimum core count required.

**Scripts + contents**

setup.R
Installs and loads all required packages. Called at the top of wrapper.R.

simulate_data.R
Functions to simulate datasets for each question.

models.R
Five stan models (3-base and 3-partner use the same model code). All models write log_lik to generated quantities for PSIS-LOO (Pareto-k).

fit_anything.R
Timed wrappers:, each attaches an elapsed_time attribute (seconds).

diagnostics.R
Utilities to extract posterior summaries and compute:

- 95% credible intervals (and mean CI width for vectors)
- RMSE vs known truth (when available)
- Pareto-k (max and proportion > 0.7) via loo

wrapper.R
Orchestrates the whole pipeline:

- create results/ and results/fits/,
- source() all helpers,
- compile Stan models once,
- loop over seeds, simulate, fit (MCMC + VB-MF + VB-FR),
- write CSV diagnostics (and optionally save fitted models).

wrapper_without_models.R
Identical to wrapper.R, is meant to be used if model outputs themselves are saved. Allows for workflow to be run without rerunning all models.

oik13154-sup-0001-appendixs1.r
The original tutorial code from the Oikos paper (kept for reference/repro).

Variational_bayes1.r
Rewritten version of the original code, only modified to use VB code.

significant_changes.txt
Notes on deviations from the original tutorial (e.g., small prior range tweaks to aid VB).

references.txt
Citations and links relevant to the project.

**Other folders:**
### HPC ###

Contains the same fundamental files as above with minor tweaks to run on the HPC, in addition to a few HPC-only scripts.

Tweaked scripts:

simulate_data_large.R
The same as simulate_data.R, modified to allow 2.2 model size to increase.

wrapper_large.R
The same as wrapper.R, modified to run only model 2.2 with increasing site/species count.

HPC-only scripts:

rerun.R
Simply sources wrapper_large.R 5 times.

plot_runtime_HPC.R
Generates a plot of HPC run times for each model, averaged out over 5 runs (plot is named "question_two_two_HPC.png").

### plotting ###

Contains all scripts used to generate figures (which are in a sub-directory called plots).

pareto_k_plot.R
Generates bar chart that shows the average proportion of Pareto-K values above 0.7 for model 2.2.

plot_CI_width.R
Generates charts that show how much wider or narrower VB CIs were when compared to MCMC.

plot_RMSE.R
Generates heatmap of RMSE values for all parameters where RMSE is applicable.

plot_runtime.R
Generates bar chart of average model runtimes in seconds for each model and method.

### results ###

Contains all results from the main analysis run from wrapper.R.

For each seed <n>:
results/param_diagnostics_seed_<n>.csv
Columns include: model, parameter, ci_lower, ci_upper, ci_width_mean,
point_estimate, RMSE, CI_True, CI_False.

results/model_diagnostics_seed_<n>.csv
Columns include: model, max_k, prop_bad_k (k > 0.7), elapsed_time_sec.
