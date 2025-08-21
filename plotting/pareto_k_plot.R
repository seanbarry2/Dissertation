# Plotting Pareto-K (model 2.2 only, all other models have near-0 Pareto-K)

library(tidyverse)

# Load all model-diagnostics files
diag_files <- list.files(
  path = "../results",
  pattern = "^model_diagnostics_seed_\\d+\\.csv$",
  full.names = TRUE
)

diag_long <- diag_files %>%
  set_names() %>%
  map_dfr(read_csv, .id = "file", show_col_types = FALSE)

# Derive method and a clean model name
pk_prepped <- diag_long %>%
  filter(!is.na(prop_bad_k)) %>%
  mutate(
    method = case_when(
      str_detect(model, "mcmc")                ~ "mcmc",
      str_detect(model, "\\bvb([.].*)?\\.fr$") ~ "vb.fr",
      str_detect(model, "\\bvb([.].*)?\\.mf$") ~ "vb.mf",
      TRUE                                     ~ NA_character_
    ),
    model_name = model %>%
      str_split("\\.") %>%
      map_chr(~ paste(.x[!.x %in% c("mcmc","vb","mf","fr")], collapse = "."))
  ) %>%
  mutate(model_name = recode(model_name,
                             "two.two" = "2.2")) %>%
  filter(model_name == "2.2")   # <- keep only model 2.2

# Compute mean proportion of bad k by method
pk_summary <- pk_prepped %>%
  filter(!is.na(method)) %>%
  group_by(model_name, method) %>%
  summarise(mean_bad_k = mean(prop_bad_k, na.rm = TRUE), .groups = "drop") %>%
  mutate(method = factor(method, levels = c("mcmc","vb.fr","vb.mf")))

# Plot
p_pk <- ggplot(pk_summary, aes(x = method, y = mean_bad_k, fill = method)) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = scales::percent(mean_bad_k, accuracy = 0.1)),
    vjust = -0.25, size = 3
  ) +
  scale_y_continuous(
    "Average proportion of Pareto-k values above 0.7",
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.08))
  ) +
  scale_x_discrete(
    labels = c("mcmc" = "MCMC", "vb.fr" = "VB-FR", "vb.mf" = "VB-MF")
  ) +
  scale_fill_manual(
    name   = "Method",
    breaks = c("mcmc", "vb.fr", "vb.mf"),
    labels = c("MCMC", "VB-FR", "VB-MF"),
    values = c("mcmc"="#f8766d","vb.fr"="#00ba38","vb.mf"="#619cff")
  ) +
  labs(x = "Method used for model 2.2") +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.y = element_text(size = 15, margin = margin(r = 30)),
    axis.title.x = element_text(size = 15, margin = margin(t = 20)),
    axis.text.x        = element_text(margin = margin(t = 20) , color = "grey25", size = 12),
    axis.text.y        = element_text(color = "grey25"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4),
    panel.grid.minor.y = element_blank(),
    legend.position    = "none"
  )

print(p_pk)

# Save
if (!dir.exists("plots")) dir.create("plots", recursive = TRUE)
ggsave("plots/avg_pareto_k_two_two.png", p_pk,
       width = 11, height = 10, dpi = 600, bg = "white")
