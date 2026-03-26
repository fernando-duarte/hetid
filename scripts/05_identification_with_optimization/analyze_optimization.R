# Analyze Optimization Results

source(here::here("scripts/utils/common_settings.R"))
load_visualization_packages()
cli_h1("Analyzing Optimization Results")

results <- readRDS(file.path(
  OUTPUT_TEMP_DIR, "identification_optimized",
  "optimized_identification_results.rds"
))
out_dir <- file.path(OUTPUT_TEMP_DIR, "identification_optimized")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
lookup <- results$lookup
join_by <- c("component" = "component_id")

# Compare baseline vs optimized widths
cli_h2("Comparing Baseline vs Optimized Widths")
baseline <- results$baseline_bounds |>
  select(-any_of(c("bond_maturity", "component_label"))) |>
  left_join(lookup, by = join_by)
optimized <- results$optimized_bounds |>
  select(-any_of(c("bond_maturity", "component_label"))) |>
  left_join(lookup, by = join_by)

width_comparison <- baseline |>
  select(
    component, component_label,
    baseline_width = width
  ) |>
  left_join(
    optimized |> select(component, optimized_width = width),
    by = "component"
  ) |>
  mutate(
    percent_reduction = (baseline_width - optimized_width) / baseline_width * 100
  )
cli_ul(c(
  paste("Mean baseline:", round(mean(width_comparison$baseline_width, na.rm = TRUE), 4)),
  paste("Mean optimized:", round(mean(width_comparison$optimized_width, na.rm = TRUE), 4)),
  paste("Mean reduction:", round(mean(width_comparison$percent_reduction, na.rm = TRUE), 2), "%")
))

# Objective function values
cli_h2("Objective Function")
total_red_pct <- (results$objective_start - results$objective_final) /
  results$objective_start * 100
cli_ul(c(
  paste("Start:", round(results$objective_start, 6)),
  paste("Final:", round(results$objective_final, 6)),
  paste("Reduction:", round(total_red_pct, 2), "%")
))

# Gamma rotation analysis
cli_h2("Gamma Rotation Analysis")
n_cols <- ncol(results$gamma_start)
cos_sim <- function(a, b) sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
gamma_similarity <- data.frame(component = seq_len(n_cols)) |>
  left_join(lookup, by = join_by)
gamma_similarity$cosine_sim <- vapply(seq_len(n_cols), function(i) {
  cos_sim(results$gamma_start[, i], results$gamma_optimized[, i])
}, numeric(1))
gamma_similarity$sign_changes <- vapply(seq_len(n_cols), function(i) {
  sum(sign(results$gamma_start[, i]) != sign(results$gamma_optimized[, i]))
}, integer(1))
cli_ul(c(
  paste("Mean cosine sim:", round(mean(gamma_similarity$cosine_sim, na.rm = TRUE), 4)),
  paste("Sign changes:", sum(gamma_similarity$sign_changes))
))

# Diagnostic flags
flagged <- width_comparison |>
  left_join(gamma_similarity |> select(component, cosine_sim), by = "component") |>
  filter(percent_reduction < 5 & cosine_sim < 0.95)
if (nrow(flagged) > 0) {
  cli_alert_warning("Components with small gains but large gamma change:")
  print(flagged)
}

# Helper for dual save
save_dual <- function(p, name) {
  ggsave(file.path(out_dir, paste0(name, ".png")),
    p,
    width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI
  )
  ggsave(file.path(out_dir, paste0(name, ".svg")),
    p,
    width = PLOT_WIDTH, height = PLOT_HEIGHT
  )
}

# Widths comparison plot
cli_h2("Creating Plots")
ct <- theme_minimal() + theme(
  plot.title = element_text(size = 14, face = "bold"),
  axis.title = element_text(size = 12)
)
plot_data <- width_comparison |>
  filter(!is.na(component_label)) |>
  pivot_longer(c(baseline_width, optimized_width), names_to = "spec", values_to = "width") |>
  mutate(spec = ifelse(spec == "baseline_width", "Baseline", "Optimized"))

p_widths <- ggplot(
  plot_data,
  aes(x = component_label, y = width, fill = spec)
) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  scale_fill_manual(
    values = c(
      "Baseline" = "#2166AC", "Optimized" = "#B2182B"
    )
  ) +
  labs(
    title = "Baseline vs Optimized Set Widths",
    x = "Component", y = "Width", fill = NULL
  ) +
  ct +
  theme(legend.position = "bottom")
save_dual(p_widths, "optimized_vs_baseline_widths")

# Gamma heatmap
gamma_tidy <- expand.grid(
  pc = seq_len(nrow(results$gamma_optimized)),
  component = seq_len(n_cols)
) |>
  mutate(loading = as.vector(results$gamma_optimized)) |>
  left_join(lookup, by = join_by)
p_heat <- ggplot(gamma_tidy, aes(x = factor(component), y = factor(pc), fill = loading)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0) +
  labs(title = "Optimized Gamma Loadings", x = "Component", y = "PC", fill = "Loading") +
  ct
save_dual(p_heat, "gamma_loadings_heatmap")

# Save analysis results
saveRDS(
  list(
    width_comparison = width_comparison,
    gamma_similarity = gamma_similarity,
    summary_stats = list(
      total_reduction_pct = total_red_pct,
      mean_cosine_sim = mean(gamma_similarity$cosine_sim, na.rm = TRUE),
      objective_start = results$objective_start,
      objective_final = results$objective_final
    )
  ),
  file.path(out_dir, "optimized_identification_analysis.rds")
)
cli_alert_success("Optimization analysis completed!")
