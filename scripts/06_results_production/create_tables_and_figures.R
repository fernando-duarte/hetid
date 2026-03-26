# Create Publication Tables and Figures

source(here::here("scripts/utils/common_settings.R"))
load_visualization_packages()
load_web_packages()
cli_h1("Creating Publication Tables and Figures")

# Load final identification results
final <- readRDS(file.path(
  OUTPUT_TEMP_DIR, "identification_results",
  "final_identification_results.rds"
))

# Set up output directory
paper_dir <- file.path(OUTPUT_PAPER_DIR, "identification")
dir.create(paper_dir, recursive = TRUE, showWarnings = FALSE)

# Extract components
comparison_table <- final$comparison_table
optimized_results <- final$optimized_results
gamma_mat <- optimized_results$gamma_optimized
n_pcs <- nrow(gamma_mat)
n_components <- ncol(gamma_mat)

save_gt <- function(tbl, base_name) {
  gtsave(tbl, file.path(paper_dir, paste0(base_name, ".html")))
  writeLines(
    as.character(as_latex(tbl)),
    file.path(paper_dir, paste0(base_name, ".tex"))
  )
}

save_plot <- function(p, base_name) {
  ggsave(file.path(paper_dir, paste0(base_name, ".png")),
    plot = p, width = PLOT_WIDTH, height = PLOT_HEIGHT,
    dpi = PLOT_DPI
  )
  ggsave(file.path(paper_dir, paste0(base_name, ".svg")),
    plot = p, width = PLOT_WIDTH, height = PLOT_HEIGHT
  )
}

cli_h2("Creating Main Identification Table")
main_df <- comparison_table |>
  select(
    bond_maturity,
    baseline_lower, baseline_upper,
    optimized_lower, optimized_upper,
    pct_width_reduction
  )

main_tbl <- gt(main_df) |>
  tab_header(title = paste(
    "Baseline vs Optimized",
    "Identified Intervals by Maturity"
  )) |>
  tab_spanner(
    label = "Baseline",
    columns = c(baseline_lower, baseline_upper)
  ) |>
  tab_spanner(
    label = "Optimized",
    columns = c(optimized_lower, optimized_upper)
  ) |>
  cols_label(
    bond_maturity = "Bond Maturity",
    baseline_lower = "Lower",
    baseline_upper = "Upper",
    optimized_lower = "Lower",
    optimized_upper = "Upper",
    pct_width_reduction = "Width Reduction (%)"
  ) |>
  fmt_number(
    columns = c(
      baseline_lower, baseline_upper,
      optimized_lower, optimized_upper
    ),
    decimals = 4
  ) |>
  fmt_number(columns = pct_width_reduction, decimals = 2)

save_gt(main_tbl, "identification_intervals_main")
cli_alert_success("Main identification table saved.")

cli_h2("Creating Gamma Loadings Table")

gamma_df <- as.data.frame(gamma_mat)
colnames(gamma_df) <- paste0(
  "Component_", seq_len(n_components)
)
gamma_df$PC <- paste0("PC", seq_len(n_pcs))
gamma_df <- gamma_df |> select(PC, everything())

gamma_tbl <- gt(gamma_df) |>
  tab_header(
    title = "Optimized Gamma Loadings",
    subtitle = "PC (rows) by Component (columns)"
  ) |>
  fmt_number(
    columns = starts_with("Component_"), decimals = 4
  )

save_gt(gamma_tbl, "optimized_gamma_appendix")
cli_alert_success("Gamma loadings table saved.")

cli_h2("Creating Interval Plot")

interval_df <- comparison_table |>
  select(
    bond_maturity,
    baseline_lower, baseline_upper,
    optimized_lower, optimized_upper
  ) |>
  tidyr::pivot_longer(
    cols = -bond_maturity,
    names_to = c("method", "bound"),
    names_sep = "_"
  ) |>
  tidyr::pivot_wider(
    names_from = bound, values_from = value
  )

p_intervals <- ggplot(
  interval_df,
  aes(
    x = bond_maturity, ymin = lower,
    ymax = upper, color = method
  )
) +
  geom_linerange(
    linewidth = 1.2,
    position = position_dodge(width = 0.3)
  ) +
  scale_color_manual(
    values = c(baseline = "blue", optimized = "red"),
    labels = c("Baseline", "Optimized")
  ) +
  labs(
    x = "Bond Maturity", y = "Theta Bounds",
    color = "Method",
    title = "Identified Intervals: Baseline vs Optimized"
  ) +
  theme_minimal()

save_plot(p_intervals, "identification_intervals")
cli_alert_success("Interval plot saved.")

cli_h2("Creating Width Reduction Bar Chart")

p_reduction <- ggplot(
  comparison_table,
  aes(x = factor(bond_maturity), y = pct_width_reduction)
) +
  geom_col(fill = "steelblue") +
  labs(
    x = "Bond Maturity", y = "Width Reduction (%)",
    title = "Interval Width Reduction by Maturity"
  ) +
  theme_minimal()

save_plot(p_reduction, "width_reduction")
cli_alert_success("Width reduction bar chart saved.")

cli_h2("Creating Gamma Heatmap")

heatmap_df <- expand.grid(
  PC = seq_len(n_pcs),
  Component = seq_len(n_components)
)
heatmap_df$Loading <- as.vector(gamma_mat)

p_heatmap <- ggplot(
  heatmap_df,
  aes(
    x = factor(Component), y = factor(PC),
    fill = Loading
  )
) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0
  ) +
  labs(
    x = "Component", y = "PC", fill = "Loading",
    title = "Optimized Gamma Loadings"
  ) +
  theme_minimal()

save_plot(p_heatmap, "gamma_heatmap")
cli_alert_success("Gamma heatmap saved.")

cli_alert_success("All tables and figures saved to: {.path {paper_dir}}")
