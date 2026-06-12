# Heteroskedasticity Diagnostics for W2 Residuals
# Legacy heteroskedasticity tests ported onto the identification-relevant W2
# residuals: the Lewbel assumption requires Var(e2 | Z) to vary with Z.

# Load required packages and settings
source(here::here("scripts/utils/common_settings.R"))
load_visualization_packages()
load_timeseries_packages()

cli_h1("Heteroskedasticity Diagnostics for W2 Residuals")

# Analysis parameters
n_pcs <- HETID_CONSTANTS$DEFAULT_N_PCS
maturities <- seq(12L, 108L, by = 12L)
significance_level <- 0.05

output_dir <- file.path(OUTPUT_TEMP_DIR, "identification_diagnostics")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Load processed data (convert list payloads to a data frame)
data <- readRDS(DATA_RDS_PATH)
if (is.list(data) && !is.data.frame(data)) {
  data <- as.data.frame(data)
}

# Identification residuals; w2 column k maps to maturities[k] by position,
# so maturity labels come from the maturities vector, never from colnames
id_resid <- compute_identification_residuals(data, maturities = maturities, n_pcs = n_pcs)
w2_resid <- id_resid$w2
pcs_aligned <- id_resid$pcs_aligned
cli_alert_info("Testing {length(maturities)} maturities, {n_pcs} PCs, {id_resid$n_obs} obs")

source(here::here("scripts/utils/hetero_lm_tests.R"))

# Regime check, then the test battery (skedastic suite + targeted LM tests).
# Regime A (default Z = PCs, inside the first-stage span) vs Regime B
# (hook-supplied Z outside it); selection logic lives in hetero_lm_tests.R.
# In Regime B the refit-residual tests diagnose the refit's variance, and
# the BP LM row (w2^2 on Z directly) is the direct relevance test.
suite_cfg <- select_diagnostics_suite(w2_resid, pcs_aligned)
regime <- suite_cfg$regime
fitted_sd_ratio <- suite_cfg$fitted_sd_ratio
suite_tests <- suite_cfg$suite_tests
gq_deflator <- suite_cfg$gq_deflator
gq_alternative <- suite_cfg$gq_alternative
all_tests <- c(suite_tests, "Glejser", "BPLM", "ARCH")
cli_alert_info(
  "Diagnostics regime {regime} (refit fitted-sd ratio
   {signif(fitted_sd_ratio, 2)}): suite = {paste(suite_tests, collapse = ', ')};
   GQ deflator = {gq_deflator}"
)

cli_h2("Running Test Battery on Every Maturity")
tests_by_maturity <- do.call(rbind, lapply(seq_along(maturities), function(k) {
  var_name <- paste0("w2_maturity_", maturities[k])
  reg_data <- data.frame(w2 = w2_resid[, k], pcs_aligned)
  lm_model <- lm(w2 ~ ., data = reg_data)
  suite <- tryCatch(
    perform_all_hetero_tests(
      lm_model, var_name,
      tests = suite_tests,
      gq_deflator = gq_deflator, gq_alternative = gq_alternative
    ),
    error = function(e) {
      cli_alert_warning("Skedastic suite failed for {var_name}: {conditionMessage(e)}")
      suite_na_row(var_name)
    }
  )
  glejser_res <- tryCatch(skedastic::glejser(lm_model), error = function(e) {
    cli_alert_warning("Glejser test failed for {var_name}: {conditionMessage(e)}")
    list(statistic = NA_real_, p.value = NA_real_)
  })
  bp_lm <- bp_lm_test(w2_resid[, k], pcs_aligned)
  arch <- arch1_test(w2_resid[, k])
  data.frame(
    maturity = maturities[k], variable = var_name,
    suite[, setdiff(names(suite), "Variable")],
    Glejser_stat = as.numeric(glejser_res$statistic),
    Glejser_pval = as.numeric(glejser_res$p.value),
    BPLM_stat = bp_lm$statistic, BPLM_df = bp_lm$df,
    BPLM_r2 = bp_lm$r_squared, BPLM_pval = bp_lm$p_value,
    ARCH_stat = arch$statistic, ARCH_pval = arch$p_value,
    stringsAsFactors = FALSE
  )
}))
rownames(tests_by_maturity) <- NULL

cli_h2("Skedastic Suite Rejection Summary")
cli_alert_info(
  "GQ is ordered by {gq_deflator} ({gq_alternative}); a p-value near 1 under the
   old time-order default meant declining variance, not homoskedasticity"
)
suite_summary <- summarize_hetero_tests(
  tests_by_maturity, significance_level,
  test_names = suite_tests
)
suite_title <- sprintf("Skedastic Suite Rejections (alpha = %.2f)", significance_level)
print(gt(suite_summary) |> tab_header(title = suite_title))

# Honesty caveat for any "k of N maturities" reading: the w2 residual series
# are highly correlated across maturities, so per-maturity rejections are one
# signal measured N ways, not N independent confirmations
w2_abs_cor <- abs(cor(w2_resid))
w2_cor_range <- range(w2_abs_cor[upper.tri(w2_abs_cor)])
cli_alert_info(
  "Cross-maturity |corr| of w2 residuals spans {round(w2_cor_range[1], 2)}
   to {round(w2_cor_range[2], 2)}: rejections across maturities are one
   signal measured {length(maturities)} ways, not independent confirmations"
)

cli_h2("Rejection Counts Across All Tests")
reject_at <- function(level) {
  unname(colSums(tests_by_maturity[paste0(all_tests, "_pval")] < level, na.rm = TRUE))
}
rejection_summary <- data.frame(
  test = all_tests, reject_1pct = reject_at(0.01),
  reject_5pct = reject_at(0.05), reject_10pct = reject_at(0.10),
  n_maturities = length(maturities)
)
print(gt(rejection_summary) |> tab_header(title = "Rejections by Significance Level"))

cli_h2("Correlations Between Instruments and Squared W2 Residuals")
# Size from the matrix the moments actually use: under HETID_Z_SOURCE the
# instrument count J can differ from the first-stage n_pcs. Default labels
# stay the historical PC names byte-for-byte; custom runs use the hook's
# column names.
n_inst <- ncol(pcs_aligned)
inst_labels <- if (z_source_active()) {
  colnames(pcs_aligned)
} else {
  paste0("PC", seq_len(n_inst))
}
corr_matrix <- matrix(NA_real_, nrow = n_inst, ncol = length(maturities))
dimnames(corr_matrix) <- list(inst_labels, paste0("maturity_", maturities))
for (j in seq_len(n_inst)) {
  for (k in seq_along(maturities)) {
    corr_matrix[j, k] <- abs(cor(pcs_aligned[, j], w2_resid[, k]^2, use = "complete.obs"))
  }
}
corr_long <- expand.grid(pc = seq_len(n_inst), maturity = maturities)
corr_long$abs_corr <- as.vector(corr_matrix)

pc_means <- sprintf("%s = %.3f", inst_labels, rowMeans(corr_matrix))
mat_means <- sprintf("m%d = %.3f", maturities, colMeans(corr_matrix))
cli_ul(c(
  paste("Mean |corr| by PC:", paste(pc_means, collapse = ", ")),
  paste("Mean |corr| by maturity:", paste(mat_means, collapse = ", "))
))
cli_h3("Strongest PC-maturity pairs")
top_pairs <- head(corr_long[order(-corr_long$abs_corr), ], 5)
cli_ul(sprintf(
  "PC%d x maturity %d: |corr| = %.4f",
  top_pairs$pc, top_pairs$maturity, top_pairs$abs_corr
))

cli_h2("Creating Diagnostic Plots")
source(here::here("scripts/utils/hetero_diag_figures.R"))
save_plot <- function(plot, name) {
  for (ext in c("png", "svg")) {
    ggsave(file.path(output_dir, paste0(name, ".", ext)), plot,
      width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI
    )
  }
}
save_plot(
  build_hetero_pvalue_figure(
    tests_by_maturity, all_tests, maturities, significance_level
  ),
  "hetero_pvalues_by_maturity"
)
save_plot(
  build_hetero_corr_heatmap(corr_long, inst_labels, maturities),
  "hetero_correlation_heatmap"
)

# Persist results
parameters <- list(
  n_pcs = n_pcs, maturities = maturities,
  significance_level = significance_level, n_obs = id_resid$n_obs,
  suite_tests = suite_tests, regime = regime,
  fitted_sd_ratio = fitted_sd_ratio,
  gq_deflator = gq_deflator, gq_alternative = gq_alternative
)
hetero_results <- list(
  parameters = parameters, tests_by_maturity = tests_by_maturity,
  correlation_matrix = corr_matrix, rejection_summary = rejection_summary,
  w2_cross_maturity_abs_cor_range = w2_cor_range
)
saveRDS(hetero_results, file.path(output_dir, "hetero_test_results.rds"))
tests_csv <- file.path(output_dir, "hetero_tests_by_maturity.csv")
write.csv(tests_by_maturity, tests_csv, row.names = FALSE)
corr_csv <- file.path(output_dir, "hetero_pc_correlations.csv")
write.csv(corr_long, corr_csv, row.names = FALSE)

cli_alert_success("Heteroskedasticity diagnostics completed!")
cli_ul(c(
  "hetero_test_results.rds", "hetero_tests_by_maturity.csv", "hetero_pc_correlations.csv",
  "hetero_pvalues_by_maturity.png / .svg", "hetero_correlation_heatmap.png / .svg"
))
cli_alert_info("All outputs saved to: {.path {output_dir}}")
