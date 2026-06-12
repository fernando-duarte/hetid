# Figure sections of create_tables_and_figures.R: interval plot, width
# reduction bar chart, and gamma heatmap. Sourced IN PLACE by
# create_tables_and_figures.R -- not standalone; expects comparison_table,
# gamma_mat, n_pcs, n_components, save_plot, and paper_dir in the calling
# environment.

cli_h2("Creating Interval Plot")

interval_df <- comparison_table |>
  select(
    component_label,
    baseline_lower, baseline_upper,
    optimized_lower, optimized_upper
  ) |>
  tidyr::pivot_longer(
    cols = -component_label,
    names_to = c("method", "bound"),
    names_sep = "_"
  ) |>
  tidyr::pivot_wider(
    names_from = bound, values_from = value
  )

# Drop unbounded intervals so geom_linerange never draws +/-Inf ranges.
n_interval_total <- nrow(interval_df)
interval_df <- interval_df |>
  filter(is.finite(lower) & is.finite(upper))
n_interval_omitted <- n_interval_total - nrow(interval_df)

p_intervals <- ggplot(
  interval_df,
  aes(
    x = component_label, ymin = lower,
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
    x = "Component", y = "Theta Bounds",
    color = "Method",
    title = "Identified Intervals: Baseline vs Optimized",
    subtitle = if (n_interval_omitted > 0) {
      paste0(n_interval_omitted, " unbounded interval(s) omitted")
    } else {
      NULL
    }
  ) +
  theme_minimal()

save_plot(p_intervals, "identification_intervals")
cli_alert_success("Interval plot saved.")

cli_h2("Creating Width Reduction Bar Chart")

# pct_width_reduction is NA where the baseline is unbounded; drop those rows so
# the bar chart never plots NA. If none remain, annotate rather than draw empty.
reduction_df <- comparison_table |>
  filter(is.finite(pct_width_reduction))

if (nrow(reduction_df) > 0) {
  p_reduction <- ggplot(
    reduction_df,
    aes(x = component_label, y = pct_width_reduction)
  ) +
    geom_col(fill = "steelblue") +
    labs(
      x = "Component", y = "Width Reduction (%)",
      title = "Interval Width Reduction by Component"
    ) +
    theme_minimal()
} else {
  p_reduction <- ggplot() +
    annotate(
      "text",
      x = 0.5, y = 0.5,
      label = "Baseline unbounded; no width reduction to report"
    ) +
    labs(title = "Interval Width Reduction by Component") +
    theme_minimal() +
    theme(
      axis.text = element_blank(), axis.title = element_blank(),
      axis.ticks = element_blank(), panel.grid = element_blank()
    )
}

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
