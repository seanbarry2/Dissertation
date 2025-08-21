# Plotting elapsed time

# Load packages
library(dplyr)
library(stringr)
library(ggplot2)
library(readr)
library(purrr)

# Gather all seeds
files <- list.files(path = "../results" ,pattern = "^model_diagnostics_seed_[0-9]+\\.csv$", full.names = TRUE)

model_dat <- files |>
  lapply(read_csv, show_col_types = FALSE) |>
  bind_rows()

# Figure out model name and method
model_dat <- model_dat |>
  mutate(
    # Identify the fitting method
    method = case_when(
      str_detect(model, "mcmc")                            ~ "mcmc",
      str_detect(model, "\\bvb([.].*)?\\.mf$")             ~ "vb.mf",
      str_detect(model, "\\bvb([.].*)?\\.fr$")             ~ "vb.fr",
      TRUE                                                 ~ NA_character_
    ),
    
    # Rebuild a clean model name (drop tokens that belong to 'method')
    model_name = model |>
      str_split("\\.") |>
      map_chr(~ paste(.x[!.x %in% c("mcmc", "vb", "mf", "fr")],
                      collapse = ".")))

# Calculate average time taken
avg_time <- model_dat |>
  group_by(model_name, method) |>
  summarise(
    mean_sec = mean(elapsed_time_sec, na.rm = TRUE),
    .groups  = "drop")

# Create the order of models from left to right
model_levels <- c(
  "one.two",
  "one.three",
  "two.one",
  "two.two",
  "three.one.base",
  "three.one.partner")

avg_time <- avg_time |>
  mutate(model_name = factor(model_name, levels = model_levels))

# Recode model names
avg_time <- avg_time |>
  mutate(model_name = recode(model_name,
                             "three.one.base"    = "three.base",
                             "three.one.partner" = "three.partner"))

# Order for the x-axis:
model_levels <- c(
  "one.two", "one.three", "two.one", "two.two", "three.base", "three.partner"
)

avg_time <- avg_time |>
  mutate(model_name = factor(model_name, levels = model_levels))

# A single dodge object so bars and labels line up perfectly
dodge <- position_dodge2(width = 1, padding = 0, preserve = "single")

avg_time <- avg_time |>
  mutate(model_name = recode(model_name,
                             "one.two"       = "1.2",
                             "one.three"     = "1.3",
                             "two.one"       = "2.1",
                             "two.two"       = "2.2",
                             "three.base"    = "3-base",
                             "three.partner" = "3-partner"
  ))

# Update factor levels in the desired order
model_levels <- c("1.2", "1.3", "2.1", "2.2", "3-base", "3-partner")
avg_time <- avg_time |>
  mutate(model_name = factor(model_name, levels = model_levels))

p <- ggplot(avg_time, aes(x = model_name, y = mean_sec, fill = method)) +
  geom_col(position = dodge, width = 0.9) +
  geom_text(
    aes(label = round(mean_sec, 1)),
    position = dodge,
    vjust = -0.25,
    size = 3
  ) +
  scale_fill_manual(
    name   = "Method",
    breaks = c("mcmc", "vb.fr", "vb.mf"),
    labels = c("MCMC", "VB-FR", "VB-MF"),
    values = c("mcmc" = "#f8766d", "vb.fr" = "#00ba38", "vb.mf" = "#619cff")
  ) +
  labs(x = "Model", y = "Mean runtime (seconds)") +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.y = element_text(size = 15, margin = margin(r = 30)),
    axis.title.x = element_text(size = 15, margin = margin(t = 20)),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5, margin = margin(t = -10), color = "grey25", size = 12),
    axis.text.y = element_text(colour = "grey25", size = 12),
    plot.title  = element_text(size = 14, face = "bold")
  ) +
  expand_limits(y = max(avg_time$mean_sec, na.rm = TRUE) * 1.1)

# Visualize before saving
print(p)

# Save plot
if (!dir.exists("plots")) dir.create("plots")
ggsave("plots/runtime.png", p,
       width = 11, height = 10, dpi = 600, bg = "white")
