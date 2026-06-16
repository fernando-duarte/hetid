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
paper_dir <- file.path(OUTPUT_TEMP_DIR, "identification")
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
# Bounds render as character via format_bound (Inf -> "unbounded",
# invalid -> "unreliable"); reduction shown as the qualitative label so an
# unbounded baseline does not produce a spurious percentage.
main_df <- comparison_table |>
  mutate(
    baseline_lower = format_bound(baseline_lower, baseline_valid_lower),
    baseline_upper = format_bound(baseline_upper, baseline_valid_upper),
    optimized_lower = format_bound(optimized_lower, optimized_valid_lower),
    optimized_upper = format_bound(optimized_upper, optimized_valid_upper)
  ) |>
  select(
    component_label,
    baseline_lower, baseline_upper,
    optimized_lower, optimized_upper,
    reduction_label
  )

main_tbl <- gt(main_df) |>
  tab_header(title = paste(
    "Baseline vs Optimized",
    "Identified Intervals by Component"
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
    component_label = "Component",
    baseline_lower = "Lower",
    baseline_upper = "Upper",
    optimized_lower = "Lower",
    optimized_upper = "Upper",
    reduction_label = "Width Reduction"
  )

save_gt(main_tbl, "identification_intervals_main")
cli_alert_success("Main identification table saved.")

cli_h2("Creating Gamma Loadings Table")

gamma_df <- as.data.frame(gamma_mat)
colnames(gamma_df) <- paste0(
  "Component_", seq_len(n_components)
)
gamma_row_names <- rownames(optimized_results$gamma_start)
gamma_df$PC <- if (is.null(gamma_row_names)) {
  paste0("PC", seq_len(n_pcs))
} else {
  gamma_row_names
}
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

source(here::here("scripts/06_results_production/create_figures_section.R"))

cli_alert_success("All tables and figures saved to: {.path {paper_dir}}")
