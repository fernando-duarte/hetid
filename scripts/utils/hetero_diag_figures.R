# Figure builders for the stage-02 heteroskedasticity diagnostics. Moved
# from heteroskedasticity_tests.R, their only consumer, to keep that script
# under the size limit.

# -log10 p-value profile across maturities for every test in the battery.
# P-values below double precision are floored for display only.
build_hetero_pvalue_figure <- function(tests_by_maturity, all_tests,
                                       maturities, significance_level) {
  plot_pvals <- tests_by_maturity |>
    select(maturity, all_of(paste0(all_tests, "_pval"))) |>
    pivot_longer(-maturity, names_to = "test", values_to = "p_value") |>
    mutate(
      test = factor(sub("_pval$", "", test), levels = all_tests),
      neg_log10_p = -log10(pmax(p_value, .Machine$double.eps))
    )
  ggplot(plot_pvals, aes(x = maturity, y = neg_log10_p, color = test)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    geom_hline(
      yintercept = -log10(significance_level),
      linetype = "dashed", color = "red"
    ) +
    geom_hline(yintercept = -log10(0.10), linetype = "dashed", color = "orange") +
    scale_x_continuous(breaks = maturities) +
    labs(
      title = "Heteroskedasticity Tests for W2 Residuals",
      x = "Maturity (months)", y = "-log10(p-value)", color = "Test",
      caption = sprintf(
        "Red line: p = %.2f, Orange line: p = 0.10", significance_level
      )
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# Heatmap of |corr(instrument_j, W2_i^2)| across instruments and maturities.
build_hetero_corr_heatmap <- function(corr_long, inst_labels, maturities) {
  heatmap_df <- corr_long
  heatmap_df$pc_label <- factor(heatmap_df$pc, labels = inst_labels)
  ggplot(heatmap_df, aes(x = maturity, y = pc_label, fill = abs_corr)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%.3f", abs_corr)), size = 3) +
    scale_fill_gradient(
      low = "white", high = "darkred", name = "|Correlation|", limits = c(0, 1)
    ) +
    scale_x_continuous(breaks = maturities) +
    labs(
      title = expression(paste(
        "Absolute Correlations: |corr(", PC[j], ", ", W[list(2, i)]^2, ")|"
      )),
      x = "Maturity (months)", y = "Principal Component"
    ) +
    theme_minimal() +
    theme(panel.grid = element_blank())
}
