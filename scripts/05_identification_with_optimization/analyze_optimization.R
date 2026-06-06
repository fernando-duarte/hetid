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
    percent_reduction = ifelse(is.finite(baseline_width),
      (baseline_width - optimized_width) / baseline_width * 100,
      NA_real_
    ),
    reduction_label = format_reduction(baseline_width, optimized_width)
  )
mean_red <- mean_pct_reduction(
  width_comparison$baseline_width, width_comparison$optimized_width
)
mean_red_str <- if (is.finite(mean_red)) {
  paste0(round(mean_red, 2), "%")
} else {
  "baseline unbounded -> optimized bounded"
}
fmt_mean <- function(x) {
  if (all(!is.finite(x))) "unbounded" else round(mean(x[is.finite(x)]), 4)
}
cli_ul(c(
  paste("Mean baseline:", fmt_mean(width_comparison$baseline_width)),
  paste("Mean optimized:", fmt_mean(width_comparison$optimized_width)),
  paste("Mean reduction:", mean_red_str)
))

# Objective function values
cli_h2("Objective Function")
start_finite <- is.finite(results$objective_start)
total_red_pct <- if (start_finite) {
  (results$objective_start - results$objective_final) /
    results$objective_start * 100
} else {
  NA_real_
}
start_str <- if (start_finite) round(results$objective_start, 6) else "unbounded"
red_str <- if (start_finite) {
  paste0(round(total_red_pct, 2), " %")
} else {
  "baseline unbounded"
}
cli_ul(c(
  paste("Start:", start_str),
  paste("Final:", round(results$objective_final, 6)),
  paste("Reduction:", red_str)
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
n_omitted <- sum(!is.finite(plot_data$width))
plot_data <- plot_data |>
  filter(is.finite(width))

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
    subtitle = if (n_omitted > 0) {
      paste0(n_omitted, " unbounded bar(s) omitted")
    } else {
      NULL
    },
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
