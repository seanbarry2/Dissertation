# Plotting RMSE as a heatmap
rm(list = ls())

# Load libraries
library(tidyverse)
library(scales)
library(stringr)

# Read all seed files
file_list <- list.files(
  path   = "../results",
  pattern = "^param_diagnostics_seed_\\d+\\.csv$",
  full.names = TRUE
)
stopifnot(length(file_list) > 0)

rmse_long <- file_list %>%
  set_names() %>%
  map_dfr(read_csv, .id = "file", show_col_types = FALSE)

stopifnot(all(c("model","parameter","RMSE") %in% names(rmse_long)))

# Robust parsing for ALL model name patterns, I used a shit naming convention 
# which is why I need to do all of this nonsense
rmse_parsed <- rmse_long %>%
  mutate(
    method = case_when(
      str_detect(model, "(^|\\.)mcmc(\\.|$)") ~ "mcmc",
      str_detect(model, "(^|\\.)vb\\.(?:.*\\.)?fr(\\.|$)") ~ "vb.fr",
      str_detect(model, "(^|\\.)vb\\.(?:.*\\.)?mf(\\.|$)") ~ "vb.mf",
      TRUE ~ NA_character_
    ),
    is_three_one = str_detect(model, "(^|\\.)three\\.one(\\.|$)"),
    side = case_when(
      is_three_one & str_detect(model, "(^|\\.)base(\\.|$)") ~ "base",
      is_three_one & str_detect(model, "(^|\\.)partner(\\.|$)") ~ "partner",
      TRUE ~ NA_character_
    ),
    core = case_when(
      is_three_one & side == "base"    ~ "three.one.base",
      is_three_one & side == "partner" ~ "three.one.partner",
      str_detect(model, "(^|\\.)one\\.two(\\.|$)")    ~ "one.two",
      str_detect(model, "(^|\\.)one\\.three(\\.|$)")  ~ "one.three",
      str_detect(model, "(^|\\.)two\\.one(\\.|$)")    ~ "two.one",
      str_detect(model, "(^|\\.)two\\.two(\\.|$)")    ~ "two.two",
      TRUE ~ NA_character_
    ),
    model_label = recode(core,
                         "one.two"="1.2","one.three"="1.3","two.one"="2.1","two.two"="2.2",
                         "three.one.base"="3-base","three.one.partner"="3-partner"
    )
  ) %>%
  filter(!is.na(method), !is.na(core), !is.na(model_label)) %>%
  mutate(method = factor(method, levels = c("mcmc","vb.fr","vb.mf")))

# Average RMSE across seeds per model × parameter × method
rmse_summary <- rmse_parsed %>%
  group_by(model_label, core, parameter, method) %>%
  summarise(mean_RMSE = mean(RMSE, na.rm = TRUE), .groups = "drop")

# Keep only RMSE parameters; drop 2.2 entirely
allowed <- tibble::tribble(
  ~model_label,  ~parameter,
  "1.2",         "spp_int",
  "1.2",         "spp_slp",
  "1.3",         "spp_int",
  "1.3",         "spp_slp",
  "2.1",         "spp_int",
  "2.1",         "spp_slp",
  "3-base",      "partner_int",
  "3-partner",   "partner_int"
)

rmse_keep <- rmse_summary %>%
  semi_join(allowed, by = c("model_label","parameter")) %>%
  filter(model_label != "2.2")

# Set order
x_order  <- c("1.2","1.3","2.1","3-base","3-partner")
y_order  <- c("spp_slp","spp_int","partner_int")

plot_df <- rmse_keep %>%
  mutate(
    method     = factor(method, levels = c("mcmc","vb.fr","vb.mf")),
    model_lab  = factor(model_label, levels = x_order),
    parameter  = factor(parameter, levels = rev(y_order))
  )

mid_val <- mean(range(plot_df$mean_RMSE, na.rm = TRUE))

# Plot
p_rmse <- ggplot(plot_df, aes(x = method, y = parameter, fill = mean_RMSE)) +
  geom_tile(width = 1, height = 1) +
  geom_text(
    aes(label = scales::number(mean_RMSE, accuracy = 0.01)),
    colour = "black", size = 3.2, fontface = "bold", family = "sans",
    show.legend = FALSE
  ) +
  facet_grid(rows = vars(model_lab), scales = "free_y", space = "free_y", switch = "y") +
  scale_x_discrete(position = "bottom",
                   labels = c(mcmc = "MCMC", `vb.fr` = "VB-FR", `vb.mf` = "VB-MF"),
                   expand = c(0, 0)) +
  scale_y_discrete(NULL, expand = c(0, 0)) +
  scale_fill_gradient(
    name   = "Mean RMSE",
    low    = "#6bff95",
    high   = "#ff5b4d",
    limits = c(0.2, 0.6)
  ) +
  labs(x = "Method") +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.y = element_text(size = 15, margin = margin(r = 30)),
    axis.title.x = element_text(size = 15, margin = margin(t = 20)),
    strip.placement  = "outside",
    strip.background = element_blank(),
    strip.text.y.left= element_text(face = "bold", margin = margin(r = 6)),
    panel.grid       = element_blank(),
    axis.ticks       = element_blank(),
    axis.text.x.top  = element_text(size = 11),
    panel.spacing.y  = unit(0.1, "lines"),
    panel.spacing.x  = unit(0, "lines")
  )

print(p_rmse)

# Save
if (!dir.exists("plots")) dir.create("plots", recursive = TRUE)
ggsave("plots/RMSE_heatmap.png", p_rmse,
       width = 11, height = 10, dpi = 600, bg = "white")
