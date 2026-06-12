# Create Theta Panel LaTeX Table
# Publication panel table of the identified sets for theta (baseline vs
# optimized) and the optimized principal component loadings, in the
# booktabs/threeparttable/siunitx format, with a standalone compilable variant

source(here::here("scripts/utils/common_settings.R"))

cli_h1("Creating Theta Panel LaTeX Table")

# Load assembled final results
final_results <- readRDS(file.path(
  OUTPUT_TEMP_DIR, "identification_results", "final_identification_results.rds"
))
ct <- final_results$comparison_table |> arrange(component)
gamma <- final_results$optimized_results$gamma_optimized
metadata <- final_results$metadata

# Identified-set panel cells via the shared four-state formatters
baseline_valid <- ct$baseline_valid_lower & ct$baseline_valid_upper
optimized_valid <- ct$optimized_valid_lower & ct$optimized_valid_upper

panel_sets <- data.frame(
  label = c(
    "Baseline lower", "Baseline upper", "Baseline width",
    "Optimized lower", "Optimized upper", "Optimized width"
  ),
  rbind(
    format_bound(ct$baseline_lower, ct$baseline_valid_lower, TABLE_DIGITS),
    format_bound(ct$baseline_upper, ct$baseline_valid_upper, TABLE_DIGITS),
    format_width(ct$baseline_width, baseline_valid, TABLE_DIGITS),
    format_bound(ct$optimized_lower, ct$optimized_valid_lower, TABLE_DIGITS),
    format_bound(ct$optimized_upper, ct$optimized_valid_upper, TABLE_DIGITS),
    format_width(ct$optimized_width, optimized_valid, TABLE_DIGITS)
  ),
  stringsAsFactors = FALSE
)

# Optimized gamma loadings panel. Rows are instruments: positional PC names
# by default (no dimnames on default gammas), the baseline's instrument
# rownames under the custom hooks.
fmt_g <- function(x) formatC(x, format = "f", digits = TABLE_DIGITS)
gamma_row_names <- rownames(final_results$optimized_results$gamma_start)
gamma_row_labels <- if (is.null(gamma_row_names)) {
  paste0("$\\gamma_{\\mathrm{PC", seq_len(nrow(gamma)), "}}$")
} else {
  paste0("$\\gamma_{\\mathrm{", gamma_row_names, "}}$")
}
panel_gamma <- data.frame(
  label = c(gamma_row_labels, "$\\sum_j \\gamma_j^2$"),
  rbind(t(apply(gamma, 1, fmt_g)), fmt_g(colSums(gamma^2))),
  stringsAsFactors = FALSE
)

theta_notes <- c(
  paste0(
    "Baseline uses the fixed ", toupper(metadata$gamma_method),
    " principal-component loadings; the optimized loadings minimize total ",
    "identified-set width via multi-start SLSQP (nloptr). Both use $\\tau = ",
    metadata$tau_baseline, "$."
  ),
  paste0(
    "``unbounded'' means the quadratic constraint admits an infinite ",
    "interval; ``unreliable'' means the solver validity check failed."
  ),
  "Loadings are unit norm by construction. Quarterly ACM data."
)

theta_table_lines <- build_panel_latex_table(
  panels = setNames(
    list(panel_sets, panel_gamma),
    c(
      paste0(
        "Identified set for $\\theta$ ($\\tau = ", metadata$tau_baseline, "$)"
      ),
      "Optimized principal component loadings $\\gamma$"
    )
  ),
  col_headers = as.character(ct$bond_maturity),
  caption = paste(
    "Identified Sets for the Heteroskedasticity Parameter and",
    "Optimized Principal Component Loadings"
  ),
  label = "tab:theta_identification",
  notes = theta_notes,
  col_group_label = "Bond maturity (months)",
  table_format = "-4.3"
)

table_paths <- write_latex_table(
  theta_table_lines,
  file.path(OUTPUT_PAPER_DIR, "identification"),
  "theta_identification_panel"
)

cli_alert_success("Theta panel table written:")
cli_ul(basename(table_paths))
cli_alert_info(
  "Outputs saved to: {.path {file.path(OUTPUT_PAPER_DIR, 'identification')}}"
)
