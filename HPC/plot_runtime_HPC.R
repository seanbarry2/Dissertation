# Sorting through question_two csvs and plotting too

# Load in packages
library(readr)
library(dplyr)
library(stringr)

# Setwd
setwd("results")

# Read the file generated with filenames + birth times
files <- read_csv("file_births.csv")

# Pull the seed out of each filename
files <- files %>% 
  mutate(seed = as.integer(str_extract(Filename, "(?<=_seed_)\\d+")))

# Drop everything with a seed between 15 and 30
# files <- files %>% 
#   filter(!(seed >= 15 & seed <= 30))

# Visualize
#View(files)

# Add column so I can manually tag runs by hand
files <- files %>% 
  mutate(run_set = NA_integer_)

# Same for tagging iteration by hand
files <- files %>% 
  mutate(iteration = NA_integer_)

# Visualize
#View(files)

# Now to change each value just use base-R indexing as follows:
files$run_set[files$seed == 15] <- 0
files$iteration[files$seed == 15] <- 15

files$run_set[files$seed == 16] <- 0
files$iteration[files$seed == 16] <- 16

files$run_set[files$seed == 17] <- 0
files$iteration[files$seed == 17] <- 17

files$run_set[files$seed == 18] <- 0
files$iteration[files$seed == 18] <- 18

files$run_set[files$seed == 19] <- 0
files$iteration[files$seed == 19] <- 19

# failed
# files$run_set[files$seed == 20] <- 0
# files$iteration[files$seed == 20] <- 20

files$run_set[files$seed == 21] <- 0
files$iteration[files$seed == 21] <- 21

files$run_set[files$seed == 22] <- 0
files$iteration[files$seed == 22] <- 22

files$run_set[files$seed == 23] <- 0
files$iteration[files$seed == 23] <- 23

files$run_set[files$seed == 24] <- 0
files$iteration[files$seed == 24] <- 24

files$run_set[files$seed == 25] <- 0
files$iteration[files$seed == 25] <- 25

files$run_set[files$seed == 26] <- 0
files$iteration[files$seed == 26] <- 26

files$run_set[files$seed == 27] <- 0
files$iteration[files$seed == 27] <- 27

files$run_set[files$seed == 28] <- 0
files$iteration[files$seed == 28] <- 28

files$run_set[files$seed == 29] <- 0
files$iteration[files$seed == 29] <- 29

files$run_set[files$seed == 30] <- 0
files$iteration[files$seed == 30] <- 30

#View(files)

files$run_set[files$seed == 3504] <- 1
files$iteration[files$seed == 3504] <- 15

files$run_set[files$seed == 4462] <- 1
files$iteration[files$seed == 4462] <- 16

files$run_set[files$seed == 1236] <- 1
files$iteration[files$seed == 1236] <- 17

files$run_set[files$seed == 5037] <- 1
files$iteration[files$seed == 5037] <- 18

files$run_set[files$seed == 8382] <- 1
files$iteration[files$seed == 8382] <- 19

files$run_set[files$seed == 1118] <- 1
files$iteration[files$seed == 1118] <- 20

files$run_set[files$seed == 7559] <- 1
files$iteration[files$seed == 7559] <- 21

files$run_set[files$seed == 4616] <- 1
files$iteration[files$seed == 4616] <- 22

files$run_set[files$seed == 7279] <- 1
files$iteration[files$seed == 7279] <- 23

files$run_set[files$seed == 8589] <- 1
files$iteration[files$seed == 8589] <- 24

files$run_set[files$seed == 5571] <- 1
files$iteration[files$seed == 5571] <- 25

files$run_set[files$seed == 8689] <- 1
files$iteration[files$seed == 8689] <- 26

files$run_set[files$seed == 4153] <- 1
files$iteration[files$seed == 4153] <- 27

files$run_set[files$seed == 4063] <- 1
files$iteration[files$seed == 4063] <- 28

files$run_set[files$seed == 2596] <- 1
files$iteration[files$seed == 2596] <- 29

files$run_set[files$seed == 265] <- 1
files$iteration[files$seed == 265] <- 30

# Visualize
#View(files)

files$run_set[files$seed == 3755] <- 2
files$iteration[files$seed == 3755] <- 15

files$run_set[files$seed == 1934] <- 2
files$iteration[files$seed == 1934] <- 16

files$run_set[files$seed == 6353] <- 2
files$iteration[files$seed == 6353] <- 17

files$run_set[files$seed == 5278] <- 2
files$iteration[files$seed == 5278] <- 18

files$run_set[files$seed == 3435] <- 2
files$iteration[files$seed == 3435] <- 19

files$run_set[files$seed == 2243] <- 2
files$iteration[files$seed == 2243] <- 20

files$run_set[files$seed == 1468] <- 2
files$iteration[files$seed == 1468] <- 21

files$run_set[files$seed == 2998] <- 2
files$iteration[files$seed == 2998] <- 22

files$run_set[files$seed == 1284] <- 2
files$iteration[files$seed == 1284] <- 23

files$run_set[files$seed == 173] <- 2
files$iteration[files$seed == 173] <- 24

files$run_set[files$seed == 5591] <- 2
files$iteration[files$seed == 5591] <- 25

files$run_set[files$seed == 6230] <- 2
files$iteration[files$seed == 6230] <- 26

files$run_set[files$seed == 4386] <- 2
files$iteration[files$seed == 4386] <- 27

files$run_set[files$seed == 4117] <- 2
files$iteration[files$seed == 4117] <- 28

files$run_set[files$seed == 421] <- 2
files$iteration[files$seed == 421] <- 29

files$run_set[files$seed == 1984] <- 2
files$iteration[files$seed == 1984] <- 30

# Visualize
#View(files)

files$run_set[files$seed == 2620] <- 3
files$iteration[files$seed == 2620] <- 15

files$run_set[files$seed == 8718] <- 3
files$iteration[files$seed == 8718] <- 16

files$run_set[files$seed == 6401] <- 3
files$iteration[files$seed == 6401] <- 17

files$run_set[files$seed == 1864] <- 3
files$iteration[files$seed == 1864] <- 18

files$run_set[files$seed == 3585] <- 3
files$iteration[files$seed == 3585] <- 19

files$run_set[files$seed == 2104] <- 3
files$iteration[files$seed == 2104] <- 20

files$run_set[files$seed == 4145] <- 3
files$iteration[files$seed == 4145] <- 21

files$run_set[files$seed == 4863] <- 3
files$iteration[files$seed == 4863] <- 22

files$run_set[files$seed == 7882] <- 3
files$iteration[files$seed == 7882] <- 23

files$run_set[files$seed == 7592] <- 3
files$iteration[files$seed == 7592] <- 24

files$run_set[files$seed == 4707] <- 3
files$iteration[files$seed == 4707] <- 25

files$run_set[files$seed == 426] <- 3
files$iteration[files$seed == 426] <- 26

files$run_set[files$seed == 4478] <- 3
files$iteration[files$seed == 4478] <- 27

files$run_set[files$seed == 5554] <- 3
files$iteration[files$seed == 5554] <- 28

files$run_set[files$seed == 1046] <- 3
files$iteration[files$seed == 1046] <- 29

files$run_set[files$seed == 4171] <- 3
files$iteration[files$seed == 4171] <- 30

# Visualize
#View(files)

files$run_set[files$seed == 9089] <- 4
files$iteration[files$seed == 9089] <- 15

files$run_set[files$seed == 3006] <- 4
files$iteration[files$seed == 3006] <- 16

files$run_set[files$seed == 3505] <- 4
files$iteration[files$seed == 3505] <- 17

files$run_set[files$seed == 3579] <- 4
files$iteration[files$seed == 3579] <- 18

files$run_set[files$seed == 2758] <- 4
files$iteration[files$seed == 2758] <- 19

files$run_set[files$seed == 1884] <- 4
files$iteration[files$seed == 1884] <- 20

files$run_set[files$seed == 2740] <- 4
files$iteration[files$seed == 2740] <- 21

files$run_set[files$seed == 2476] <- 4
files$iteration[files$seed == 2476] <- 22

files$run_set[files$seed == 6451] <- 4
files$iteration[files$seed == 6451] <- 23

files$run_set[files$seed == 2269] <- 4
files$iteration[files$seed == 2269] <- 24

files$run_set[files$seed == 4435] <- 4
files$iteration[files$seed == 4435] <- 25

# Stopped running
# files$run_set[files$seed == 6230] <- 4
# files$iteration[files$seed == 6230] <- 26
# 
# files$run_set[files$seed == 4386] <- 4
# files$iteration[files$seed == 4386] <- 27
# 
# files$run_set[files$seed == 4117] <- 4
# files$iteration[files$seed == 4117] <- 28
# 
# files$run_set[files$seed == 421] <- 4
# files$iteration[files$seed == 421] <- 29
# 
# files$run_set[files$seed == 1984] <- 4
# files$iteration[files$seed == 1984] <- 30

# Marrying the two datasets
# Keep only the “model diagnostics” files (skip the param ones)
model_files <- files %>% 
  filter(str_detect(Filename, "model_diagnostics"))

# For safety, convert relative paths if needed
model_files <- model_files %>% 
  mutate(full_path = file.path("results", basename(Filename)))  # adjust folder if wrong

# Pull the runtime out of each CSV and attach run_set / iteration
runtime_df <- model_files %>%
  mutate(
    full_path = normalizePath(Filename, winslash = "/", mustWork = TRUE),
    data      = map(full_path, ~ read_csv(.x, show_col_types = FALSE))
  ) %>% 
  unnest(data) %>% 
  select(seed, run_set, iteration, model, elapsed_time_sec)

# Plotting!
# Averaging out values
n_runs <- n_distinct(runtime_df$run_set)

runtime_avg <- runtime_df %>%
  group_by(iteration, model) %>%
  summarise(
    mean_time_sec = mean(elapsed_time_sec, na.rm = TRUE),
    sd_time_sec   = sd(elapsed_time_sec,   na.rm = TRUE),
    n             = n(),
    .groups       = "drop"
  ) %>% 
  mutate(
    se_time_sec  = sd_time_sec / sqrt(n),
    ci95_sec     = se_time_sec * qt(0.975, df = pmax(n - 1, 1))
  )


# Averaged plot
single_model <- n_distinct(runtime_avg$model) == 1

avg_plot <- ggplot(
  runtime_avg,
  aes(x = iteration,
      y = mean_time_sec / 3600, # hours
      group   = model,
      colour  = if (single_model) NULL else model)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous("Model size", breaks = 15:30) +
  scale_y_continuous("Mean elapsed time (hours)",
                     labels = scales::label_number(accuracy = 0.1)) +
  labs(colour = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    axis.ticks.x   = element_line(),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom"
  )

print(avg_plot)

# Save plot
ggsave("question_two_HPC.png",
       avg_plot, width = 8, height = 6, dpi = 500, bg = "white")

# For inspecting differences in runtime
# Parse method from the model column
runtime_by_method <- runtime_df %>%
  mutate(
    method = case_when(
      str_detect(model, "(^|\\.)mcmc(\\.|$)") ~ "MCMC",
      str_detect(model, "(^|\\.)vb\\.(?:.*\\.)?mf(\\.|$)") ~ "VB-MF",
      str_detect(model, "(^|\\.)vb\\.(?:.*\\.)?fr(\\.|$)") ~ "VB-FR",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(method))

# Average computing time per iteration × method
time_iter_method <- runtime_by_method %>%
  group_by(iteration, method) %>%
  summarise(
    mean_time_sec = mean(elapsed_time_sec, na.rm = TRUE),
    sd_time_sec   = sd(elapsed_time_sec,   na.rm = TRUE),
    n             = dplyr::n(),
    .groups = "drop"
  ) %>%
  mutate(
    se_time_sec  = sd_time_sec / sqrt(pmax(n, 1)),
    ci95_sec     = qt(0.975, df = pmax(n - 1, 1)) * se_time_sec,
    mean_time_min = mean_time_sec / 60,
    mean_time_hr  = mean_time_sec / 3600,
    ci95_min      = ci95_sec / 60,
    ci95_hr       = ci95_sec / 3600
  )

# Table for quick inspection
time_iter_method_wide <- time_iter_method %>%
  select(iteration, method, mean_time_min) %>%
  pivot_wider(names_from = method, values_from = mean_time_min) %>%
  arrange(iteration)

print(time_iter_method_wide)