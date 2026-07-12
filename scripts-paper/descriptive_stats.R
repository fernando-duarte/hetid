# Descriptive statistics for consumption growth, the SDF PCs, and the SDF news
# and expected SDF series at selected maturities: merge the series over the
# date_begin ~ date_end window, report missing date+variable combinations,
# write the summary-statistics, regression, and correlation LaTeX tables and
# the time-series and histogram figures to scripts-paper/output/, then compile
# everything into descriptive_stats.pdf via latexmk.
# Run via run_all.R after the data scripts.

# sources of the merged panel, named by series group (the names drive the
# facet colors below)
panel_sources <- list(
  "consumption growth" = gr1_pcecc96,
  "expected SDF PCs" = expected_sdf_pc,
  "lagged expected SDF PCs" = lag_expected_sdf_pc,
  "SDF news PCs" = sdf_news_pc,
  "expected SDF" = expected_sdf[, c("qtr", paste0(expected_prefix, show_mats))],
  "SDF news" = sdf_news[, c("qtr", paste0(news_prefix, show_mats))]
)
panel <- panel_sources |>
  purrr::reduce(dplyr::full_join, by = "qtr") |>
  filter_window() |>
  dplyr::arrange(qtr)

panel_long <- panel |>
  tidyr::pivot_longer(-qtr, names_to = "variable") |>
  dplyr::mutate(variable = factor(variable, levels = value_cols(panel)))

# report date+variable combinations with missing values
nas <- dplyr::filter(panel_long, is.na(value))
if (nrow(nas) == 0) {
  cat("no missing date+variable combinations over", date_begin, "to", date_end, "\n")
} else {
  cat("missing date+variable combinations:\n")
  print(dplyr::select(nas, qtr, variable), n = Inf)
}

# summary statistics, one row per series
summary_stats <- panel_long |>
  dplyr::summarise(
    n = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    sd = stats::sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    median = stats::median(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    .by = variable
  )
# output file names (the wrapper references the basenames, so they live once)
summary_tex <- "summary_stats.tex"
correlations_tex <- "correlations.tex"
regression_tex <- "ols_mean_eq.tex"
figures_pdf <- "figures.pdf"
wrapper_tex <- file.path(out_dir, "descriptive_stats.tex")

# write a data frame as a booktabs LaTeX table into out_dir
write_kable <- function(x, file, digits) {
  writeLines(
    knitr::kable(x, format = "latex", booktabs = TRUE, digits = digits),
    file.path(out_dir, file)
  )
}

write_kable(summary_stats, summary_tex, digits = 3)

# correlation matrix
corr <- stats::cor(panel[value_cols(panel)], use = "pairwise.complete.obs")
write_kable(corr, correlations_tex, digits = 2)

# regression of consumption growth on the lagged expected-SDF and news PCs
reg_summary <- summary(ols_mean_eq)
regression_stats <- tibble::as_tibble(reg_summary$coefficients, rownames = "term")
names(regression_stats) <- c("term", "estimate", "std. error", "t", "p value")
write_kable(regression_stats, regression_tex, digits = 3)

# figures: one page of time series, one page of histograms, colored by series
# group (palette hues validated for colorblind separation; facet strips name
# each series, so no legend is needed)
group_colors <- c(
  "consumption growth" = "#2a78d6",
  "expected SDF PCs" = "#1baf7a",
  "SDF news PCs" = "#eda100",
  "expected SDF" = "#008300",
  "SDF news" = "#4a3aa7",
  "lagged expected SDF PCs" = "#e34948"
)
# each variable's group comes from the panel_sources frame it belongs to
variable_group <- purrr::list_c(purrr::imap(
  panel_sources,
  \(df, group) stats::setNames(rep(group, length(value_cols(df))), value_cols(df))
))
plot_df <- panel_long |>
  dplyr::mutate(
    date = as.Date(qtr),
    group = unname(variable_group[as.character(variable)])
  )
ts_plot <- ggplot2::ggplot(plot_df, ggplot2::aes(date, value, color = group)) +
  ggplot2::geom_line() +
  ggplot2::scale_color_manual(values = group_colors, guide = "none") +
  ggplot2::facet_wrap(~variable, scales = "free_y", ncol = 3) +
  ggplot2::labs(x = NULL, y = NULL)
hist_plot <- ggplot2::ggplot(plot_df, ggplot2::aes(value, fill = group)) +
  ggplot2::geom_histogram(bins = 30) +
  ggplot2::scale_fill_manual(values = group_colors, guide = "none") +
  ggplot2::facet_wrap(~variable, scales = "free", ncol = 3) +
  ggplot2::labs(x = NULL, y = NULL)
grDevices::pdf(file.path(out_dir, figures_pdf), width = 11, height = 8.5)
print(ts_plot)
print(hist_plot)
grDevices::dev.off()

# assemble tables and figures into one pdf
writeLines(c(
  "\\documentclass{article}",
  "\\usepackage[margin=1in]{geometry}",
  "\\usepackage{booktabs}",
  "\\usepackage{graphicx}",
  "\\usepackage{pdflscape}",
  "\\begin{document}",
  "\\section*{Summary statistics}",
  "\\begin{center}",
  paste0("\\input{", summary_tex, "}"),
  "\\end{center}",
  "\\section*{Consumption growth regression}",
  "\\begin{center}",
  paste0("\\input{", regression_tex, "}"),
  "\\end{center}",
  paste0(
    "\\noindent Dependent variable: quarterly real consumption growth (percent). ",
    "$N = ", stats::nobs(ols_mean_eq),
    "$, $R^2 = ", round(reg_summary$r.squared, 3),
    "$, adjusted $R^2 = ", round(reg_summary$adj.r.squared, 3), "$."
  ),
  "\\begin{landscape}",
  "\\section*{Correlation matrix}",
  "\\begin{center}",
  paste0("\\resizebox{\\linewidth}{!}{\\input{", correlations_tex, "}}"),
  "\\end{center}",
  "\\end{landscape}",
  "\\section*{Time series}",
  paste0("\\noindent\\includegraphics[width=\\textwidth,page=1]{", figures_pdf, "}"),
  "\\section*{Histograms}",
  paste0("\\noindent\\includegraphics[width=\\textwidth,page=2]{", figures_pdf, "}"),
  "\\end{document}"
), wrapper_tex)
status <- system2(
  "latexmk", c("-cd", "-pdf", "-silent", wrapper_tex),
  stdout = FALSE, stderr = FALSE
)
if (status != 0) stop("latexmk failed on ", wrapper_tex, " (status ", status, ")")
invisible(system2(
  "latexmk", c("-cd", "-c", wrapper_tex),
  stdout = FALSE, stderr = FALSE
))

rm(
  panel_sources, variable_group, panel_long, nas, group_colors, plot_df,
  ts_plot, hist_plot, reg_summary, write_kable, summary_tex, correlations_tex,
  regression_tex, figures_pdf, wrapper_tex, status
)
