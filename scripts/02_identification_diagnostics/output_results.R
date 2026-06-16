# Export Identification Diagnostics Results
# Publication LaTeX panel table (plus standalone variant) and HTML mirrors of
# the W2 heteroskedasticity tests, n-hat episode exports, and figure copies

source(here::here("scripts/utils/common_settings.R"))
load_web_packages()

cli_h1("Exporting Identification Diagnostics")

# Load diagnostics computed by the upstream stage scripts
temp_dir <- file.path(OUTPUT_TEMP_DIR, "identification_diagnostics")
hetero <- readRDS(file.path(temp_dir, "hetero_test_results.rds"))
n_hat <- readRDS(file.path(temp_dir, "n_hat_episode_results.rds"))

paper_dir <- file.path(OUTPUT_TEMP_DIR, "identification_diagnostics")
figures_dir <- file.path(paper_dir, "figures")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

# Heteroskedasticity panel table
cli_h2("Creating Heteroskedasticity Panel Table")

tbm <- hetero$tests_by_maturity
tbm <- tbm[order(tbm$maturity), ]
corr_mat <- hetero$correlation_matrix
n_pcs <- hetero$parameters$n_pcs
n_obs <- hetero$parameters$n_obs

fmt_p <- function(x) {
  ifelse(is.na(x), NA_character_, formatC(x, format = "f", digits = 3))
}

# Regime-aware panel rows, labels, and notes (suite as actually run,
# GQ deflator label, Regime-B reading guidance) come from the shared
# metadata helper
source(here::here("scripts/utils/hetero_panel_meta.R"))
meta <- hetero_suite_meta(hetero$parameters, corr_mat)

panel_suite <- data.frame(
  label = meta$suite_labels,
  t(vapply(tbm[, meta$suite_pval_cols], fmt_p, character(nrow(tbm)))),
  stringsAsFactors = FALSE
)
panel_targeted <- data.frame(
  label = c(meta$bplm_label, "ARCH(1)"),
  rbind(fmt_p(tbm$BPLM_pval), fmt_p(tbm$ARCH_pval)),
  stringsAsFactors = FALSE
)
panel_corr <- data.frame(
  label = meta$corr_labels,
  fmt_p(corr_mat),
  stringsAsFactors = FALSE
)

hetero_notes <- hetero_table_notes(hetero, meta, n_pcs, n_obs, corr_mat, tbm)

panel_list <- list(
  "Conditional heteroskedasticity tests (p-values)" = panel_suite,
  "Identification-targeted tests (p-values)" = panel_targeted,
  panel_corr
)
names(panel_list)[3] <- paste0(
  "Absolute correlation $|\\mathrm{corr}(", meta$inst_symbol,
  "_j, W_{2,i}^2)|$"
)
hetero_table_lines <- build_panel_latex_table(
  panels = panel_list,
  col_headers = as.character(tbm$maturity),
  caption = "Heteroskedasticity Diagnostics for Identification Residuals",
  label = "tab:hetero_diagnostics",
  notes = hetero_notes
)
hetero_table_paths <- write_latex_table(
  hetero_table_lines, paper_dir, "heteroskedasticity_tests_table"
)
cli_alert_success("LaTeX panel table written (fragment + standalone)")

# HTML mirror of the p-value matrix
cli_h2("Creating HTML Tables")

pval_matrix <- t(as.matrix(tbm[, c(meta$suite_pval_cols, "BPLM_pval", "ARCH_pval")]))
pval_df <- data.frame(
  Test = c(
    gsub("--", "-", meta$suite_labels),
    sub(" \\(.*", "", meta$bplm_label), "ARCH(1)"
  ),
  pval_matrix,
  check.names = FALSE,
  row.names = NULL
)
names(pval_df)[-1] <- as.character(tbm$maturity)

hetero_html <- pval_df |>
  gt() |>
  tab_header(
    title = "Heteroskedasticity Diagnostics for Identification Residuals",
    subtitle = "P-values by bond maturity (W2 residualized on the common X_t)"
  ) |>
  fmt_number(columns = -Test, decimals = 3) |>
  tab_spanner(label = "Maturity (months)", columns = -Test)
gtsave(hetero_html, file.path(paper_dir, "heteroskedasticity_tests_table.html"))

episodes_html <- n_hat$episodes |>
  gt() |>
  tab_header(
    title = "Positive n-hat Episodes",
    subtitle = "Contiguous positive n-hat periods (monthly ACM data)"
  ) |>
  fmt_number(columns = where(is.numeric), decimals = 4)
gtsave(episodes_html, file.path(paper_dir, "n_hat_episodes_table.html"))
cli_alert_success("HTML tables written")

# Machine-readable n-hat exports
cli_h2("Copying n-hat CSV Exports")
for (csv_file in c("n_hat_episodes.csv", "n_hat_prediction_validation.csv")) {
  file.copy(
    file.path(temp_dir, csv_file),
    file.path(paper_dir, csv_file),
    overwrite = TRUE
  )
}
cli_alert_success("CSV exports copied")

# Copy figures to the paper directory
cli_h2("Copying Figures")
diagnostic_figures <- c(
  "hetero_pvalues_by_maturity.png", "hetero_correlation_heatmap.png",
  "n_hat_timeline.png", "n_hat_prediction_scatter.png"
)
for (plot_file in diagnostic_figures) {
  src <- file.path(temp_dir, plot_file)
  if (file.exists(src)) {
    file.copy(src, file.path(figures_dir, plot_file), overwrite = TRUE)
  }
}
cli_alert_success("Figures copied to paper directory")

# Summary of exports
cli_h2("Export Summary")
cli_ul(c(
  paste("LaTeX tables:", paste(basename(hetero_table_paths), collapse = ", ")),
  "HTML tables: heteroskedasticity_tests_table.html, n_hat_episodes_table.html",
  "CSV exports: n_hat_episodes.csv, n_hat_prediction_validation.csv",
  paste("Figures:", paste(diagnostic_figures, collapse = ", "))
))
cli_alert_success("Identification diagnostics export completed!")
cli_alert_info("Outputs saved to: {.path {paper_dir}}")
