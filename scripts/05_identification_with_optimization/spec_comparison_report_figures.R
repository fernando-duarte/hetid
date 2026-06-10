# Figure writers for the spec-comparison report (png + svg). Requires
# common_settings.R, the visualization packages, and
# spec_comparison_report_utils.R to be sourced first.

# Long computed subtitles (coverage lines) must wrap, not clip at the panel.
wrap_spec_text <- function(x, width = 90) {
  vapply(x, function(s) paste(strwrap(s, width = width), collapse = "\n"), "")
}

SPEC_OPT_CAVEAT <- paste(
  "Optimized weights are a width-minimizing computational benchmark,",
  "not an economically structural weighting."
)

save_spec_figure <- function(p, paper_dir, base, suffix) {
  ggsave(file.path(paper_dir, paste0(base, suffix, ".png")), p,
    width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI
  )
  ggsave(file.path(paper_dir, paste0(base, suffix, ".svg")), p,
    width = PLOT_WIDTH, height = PLOT_HEIGHT
  )
}

write_spec_outcomes_figure <- function(grid, cov, bl, paper_dir, suffix) {
  d <- grid
  d$scheme <- factor(
    unname(SPEC_SCHEME_LABELS[d$gamma]),
    levels = rev(unname(SPEC_SCHEME_LABELS))
  )
  d$tau_facet <- factor(
    paste("tau =", d$tau),
    levels = paste("tau =", sort(unique(d$tau)))
  )
  p <- ggplot(d, aes(y = scheme, fill = outcome)) +
    geom_bar(width = 0.7) +
    geom_text(
      aes(label = after_stat(count)),
      stat = "count", position = position_stack(vjust = 0.5),
      size = 3, color = "white"
    ) +
    facet_wrap(~tau_facet, nrow = 1) +
    scale_fill_manual(values = SPEC_OUTCOME_FILL) +
    labs(
      title = wrap_spec_text(bl$short), subtitle = wrap_spec_text(cov$line),
      caption = SPEC_OPT_CAVEAT,
      x = "Specifications (count)", y = NULL, fill = "Outcome"
    ) +
    guides(fill = guide_legend(nrow = 2)) +
    theme_minimal() +
    theme(legend.position = "bottom", plot.title.position = "plot")
  save_spec_figure(p, paper_dir, "spec_comparison_outcomes", suffix)
}

write_spec_widths_figure <- function(grid, cov, paper_dir, suffix) {
  pos <- grid[grid$tau > 0, ]
  pos$stratum <- spec_stratum_label(pos$mode, pos$components)
  fin <- pos[pos$outcome == "certified bounded", ]
  if (!nrow(fin)) {
    cli_alert_warning("No certified bounded tau > 0 cells; widths figure skipped")
    return(invisible(NULL))
  }
  # Censoring is never silent: omitted counts live in each panel's label.
  panel_label <- vapply(split(pos, pos$stratum), function(d) {
    sprintf(
      "%s\n(omitted: %d unbounded, %d no certified bound)", d$stratum[1],
      sum(d$outcome == "certified unbounded"), sum(d$outcome == "no certified bound")
    )
  }, "")
  fin$panel <- panel_label[fin$stratum]
  fin$scheme <- factor(
    unname(SPEC_SCHEME_LABELS[fin$gamma]),
    levels = unname(SPEC_SCHEME_LABELS)
  )
  p <- ggplot(fin, aes(factor(tau), width, color = scheme)) +
    geom_jitter(width = 0.12, height = 0, size = 2, alpha = 0.85) +
    scale_y_log10() +
    facet_wrap(~panel, scales = "free_y") +
    labs(
      title = sprintf(
        "Solver-certified finite widths: %d of %d tau > 0 cells", nrow(fin), nrow(pos)
      ),
      subtitle = paste0(
        wrap_spec_text(cov$line),
        "\nWidths are comparable only within a panel (stratum); ",
        "scales differ by orders of magnitude across panels."
      ),
      caption = SPEC_OPT_CAVEAT,
      x = "Slack tau", y = "Identified-set width (log scale)", color = "Scheme"
    ) +
    guides(color = guide_legend(nrow = 2)) +
    theme_minimal() +
    theme(legend.position = "bottom", plot.title.position = "plot")
  save_spec_figure(p, paper_dir, "spec_comparison_widths", suffix)
}
