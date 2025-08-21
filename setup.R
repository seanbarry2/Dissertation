# Setup script that installs and loads all necessary packages and checks if all files are there

# List of packages needed
pkgs <- c("dplyr", "geiger", "mvtnorm", "pez", "lme4", "rstan", "loo")

# Install any that are missing
new <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if (length(new)) install.packages(new, dependencies = TRUE)

# load all of them
invisible(lapply(pkgs, library, character.only = TRUE))