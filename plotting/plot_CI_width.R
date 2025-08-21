# Plotting CI-width ratios: VB vs MCMC

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

ci_long_all <- file_list %>%
  set_names() %>%
  map_dfr(read_csv, .id = "file", show_col_types = FALSE)

width_col <- if ("ci_width_mean" %in% names(ci_long_all)) "ci_width_mean"

# Robust parsing for ALL model name patterns, I used a shit naming convention 
# which is why I need to do all of this nonsense
parsed <- ci_long_all %>%
  mutate(
    # detect method robustly
    method = case_when(
      str_detect(model, "(^|\\.)mcmc(\\.|$)") ~ "mcmc",
      str_detect(model, "(^|\\.)vb\\.(?:.*\\.)?fr(\\.|$)") ~ "vb.fr",
      str_detect(model, "(^|\\.)vb\\.(?:.*\\.)?mf(\\.|$)") ~ "vb.mf",
      TRUE ~ NA_character_
    ),
    # detect core (handle three.one side regardless of order)
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
                         "one.two"            = "1.2",
                         "one.three"          = "1.3",
                         "two.one"            = "2.1",
                         "two.two"            = "2.2",
                         "three.one.base"     = "3-base",
                         "three.one.partner"  = "3-partner"
    )
  ) %>%
  filter(!is.na(core), !is.na(method)) %>%
  mutate(method = factor(method, levels = c("mcmc","vb.fr","vb.mf")))

# Average CI width across seeds per model × parameter × method
ci_summary <- parsed %>%
  group_by(model_label, core, parameter, method) %>%
  summarise(mean_ci_width = mean(.data[[width_col]], na.rm = TRUE), .groups = "drop")

# Compute VB/MCMC ratios
ci_wide <- ci_summary %>%
  pivot_wider(names_from = method, values_from = mean_ci_width) %>%
  mutate(
    ratio_vb_fr = if_else(!is.na(`vb.fr`) & !is.na(mcmc), `vb.fr` / mcmc, NA_real_),
    ratio_vb_mf = if_else(!is.na(`vb.mf`) & !is.na(mcmc), `vb.mf` / mcmc, NA_real_)
  )

# Long-form for plotting
ratio_long <- ci_wide %>%
  select(model_label, parameter, ratio_vb_fr, ratio_vb_mf) %>%
  pivot_longer(starts_with("ratio_"), names_to = "vb_method", values_to = "ratio") %>%
  mutate(
    method = recode(vb_method,
                    "ratio_vb_fr" = "vb.fr",
                    "ratio_vb_mf" = "vb.mf"
    ),
    method = factor(method, levels = c("vb.fr","vb.mf"))
  ) %>%
  filter(is.finite(ratio), ratio > 0)

# Plot as 2x3
facet_order <- c("1.2","1.3","2.1","2.2","3-base","3-partner")
plot_df <- ratio_long %>%
  mutate(
    model_label = factor(model_label, levels = facet_order),
    parameter   = fct_inorder(parameter)
  )

plot_df_log <- ratio_long %>%
  filter(is.finite(ratio), ratio > 0) %>%
  mutate(
    model_label = factor(model_label, levels = c("1.2","1.3","2.1","2.2","3-base","3-partner")),
    parameter   = fct_inorder(parameter),
    log2_ratio  = log2(ratio)
  )

p_ratio <- ggplot(plot_df_log, aes(x = parameter, y = log2_ratio, colour = method)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "#f8766d") +
  geom_point(size = 2.25, position = position_dodge(width = 0.05)) +
  scale_colour_manual(
    name   = "Method",
    breaks = c("vb.fr","vb.mf"),
    labels = c("VB-FR","VB-MF"),
    values = c("vb.fr"="#00ba38","vb.mf"="#619cff")
  ) +
  scale_y_continuous(
    name   = "Relative CI width (log₂ VB / MCMC)",
    limits = c(-2, 4.75),
    breaks = -2:4,
    labels = -2:4,
    expand = expansion(mult = c(0.05, 0.1))
  ) +
  expand_limits(y = c(-2, 2)
  ) +
  labs(
    x = "Parameters"
  ) +
  facet_wrap(~ model_label, ncol = 2, nrow = 3, scales = "free_x", drop = FALSE) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.y = element_text(size = 15, margin = margin(r = 30)),
    axis.title.x = element_text(size = 15, margin = margin(t = 20)),
    legend.position    = "bottom",
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 7.25),
    panel.grid.major.x = element_blank(),
    plot.title  = element_text(size = 14, face = "bold")
  )

print(p_ratio)

# Save
if (!dir.exists("plots")) dir.create("plots", recursive = TRUE)
ggsave("plots/ci_width_ratios.png", p_ratio,
       width = 11, height = 10, dpi = 600, bg = "white")
