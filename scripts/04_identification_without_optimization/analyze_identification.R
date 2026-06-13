# Analyze Baseline Identification Results

source(here::here("scripts/utils/common_settings.R"))
load_visualization_packages()
cli_h1("Analyzing Baseline Identification Results")

results <- readRDS(file.path(
  OUTPUT_TEMP_DIR, "identification_baseline",
  "baseline_identification_results.rds"
))
out_dir <- file.path(OUTPUT_TEMP_DIR, "identification_baseline")
lookup <- results$lookup
join_by <- c("component" = "component_id")

# Merge bounds with lookup for labeling
bounds_tau0 <- results$bounds_tau0 |>
  select(-any_of(c("bond_maturity", "component_label"))) |>
  left_join(lookup, by = join_by) |>
  mutate(tau_spec = "tau = 0")
bounds_tau_set <- results$bounds_tau_set |>
  select(-any_of(c("bond_maturity", "component_label"))) |>
  left_join(lookup, by = join_by) |>
  mutate(tau_spec = "tau = 0.05")
bounds_combined <- bind_rows(bounds_tau0, bounds_tau_set)

# Width summaries
cli_h2("Width Summaries")
width_summary <- bounds_combined |>
  group_by(tau_spec) |>
  summarise(
    mean_width = mean(width, na.rm = TRUE),
    median_width = median(width, na.rm = TRUE),
    min_width = min(width, na.rm = TRUE),
    max_width = max(width, na.rm = TRUE),
    sd_width = sd(width, na.rm = TRUE),
    n_components = n(), .groups = "drop"
  )
# Render Inf widths as "unbounded" rather than printing round(Inf).
fmt_mean_width <- function(w) {
  if (all(!is.finite(w))) {
    "unbounded"
  } else {
    paste0(
      round(mean(w[is.finite(w)]), 4),
      if (any(!is.finite(w))) " (some unbounded)" else ""
    )
  }
}
cli_ul(c(
  paste("tau=0 mean width:", fmt_mean_width(bounds_tau0$width)),
  paste("tau=0.05 mean width:", fmt_mean_width(bounds_tau_set$width))
))

# Compare point vs set identification widths
comparison <- bounds_tau0 |>
  select(
    component, component_label,
    lower_tau0 = lower, upper_tau0 = upper, width_tau0 = width
  ) |>
  left_join(
    bounds_tau_set |>
      select(
        component,
        lower_tset = lower,
        upper_tset = upper, width_tset = width
      ),
    by = "component"
  ) |>
  mutate(
    width_ratio = width_tset / width_tau0,
    width_diff = width_tset - width_tau0
  )

# Boundedness / validity diagnostics. Report % bounded and % valid among
# bounded SEPARATELY: an unbounded side carries valid = TRUE, so averaging
# validity over unbounded rows would falsely read as "perfect".
cli_h2("Numerical Diagnostics")
summarise_flags <- function(bnds) {
  bounded <- bnds$bounded_lower & bnds$bounded_upper
  pct_bounded <- mean(bounded)
  pct_valid <- if (any(bounded)) {
    mean((bnds$valid_lower & bnds$valid_upper)[bounded])
  } else {
    NA_real_
  }
  data.frame(pct_bounded = pct_bounded, pct_valid_among_bounded = pct_valid)
}
conv_tau0 <- summarise_flags(bounds_tau0)
conv_tset <- summarise_flags(bounds_tau_set)
fmt_pct <- function(x) if (is.na(x)) "n/a" else round(x, 3)
cli_alert_info(paste0(
  "tau=0 bounded: {.val {fmt_pct(conv_tau0$pct_bounded)}}, ",
  "valid|bounded: {.val {fmt_pct(conv_tau0$pct_valid_among_bounded)}}"
))
cli_alert_info(paste0(
  "tau=0.05 bounded: {.val {fmt_pct(conv_tset$pct_bounded)}}, ",
  "valid|bounded: {.val {fmt_pct(conv_tset$pct_valid_among_bounded)}}"
))

# Eigenvalue diagnostics
min_eig <- vapply(results$quadratic_tau_set$quadratic$A_i, function(a) {
  min(eigen(a, symmetric = TRUE, only.values = TRUE)$values)
}, numeric(1))
eig_df <- data.frame(
  component = seq_along(min_eig), min_eigenvalue = min_eig
) |> left_join(lookup, by = join_by)
cli_ul(c(
  paste("Min eigenvalue:", round(min(min_eig), 6)),
  paste("Negative count:", sum(min_eig < 0))
))

# Helper to save plots as PNG and SVG
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

# Widths by maturity plot
cli_h2("Creating Plots")
ct <- theme_minimal() + theme(
  plot.title = element_text(size = 14, face = "bold"),
  axis.title = element_text(size = 12)
)

# Drop unbounded (non-finite width) rows so the plot never receives Inf.
plot_widths <- bounds_combined |>
  filter(!is.na(component_label), is.finite(width))
n_unbounded <- sum(!is.finite(bounds_combined$width), na.rm = TRUE)
p_widths <- ggplot(
  plot_widths,
  aes(x = component_label, y = width, fill = tau_spec)
) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  scale_fill_manual(values = c("tau = 0" = "#2166AC", "tau = 0.05" = "#B2182B")) +
  labs(
    title = "Identified Set Widths by Maturity",
    subtitle = if (n_unbounded > 0) {
      paste0(n_unbounded, " component(s) unbounded (omitted)")
    } else {
      NULL
    },
    x = "Component", y = "Width", fill = "Specification"
  ) +
  ct +
  theme(legend.position = "bottom")
save_dual(p_widths, "baseline_widths_by_maturity")

# Eigenvalue diagnostics plot
p_eig <- ggplot(
  eig_df |> filter(!is.na(component_label)),
  aes(x = component_label, y = min_eigenvalue)
) +
  geom_col(fill = "#4393C3", width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Minimum Eigenvalue of A_i by Maturity",
    x = "Component", y = "Minimum Eigenvalue"
  ) +
  ct
save_dual(p_eig, "baseline_quadratic_diagnostics")

# Save analysis results
diagnostics <- list(
  eigenvalues = eig_df,
  convergence_tau0 = conv_tau0,
  convergence_tau_set = conv_tset
)
saveRDS(
  list(
    bounds_combined = bounds_combined,
    width_summary = width_summary,
    diagnostics = diagnostics,
    comparison = comparison
  ),
  file.path(out_dir, "baseline_identification_analysis.rds")
)
cli_alert_success("Baseline analysis completed!")
