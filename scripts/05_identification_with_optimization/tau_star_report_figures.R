# Figure and table builders for the tau* report: the sanitized for_paper
# sweep and the overlay / blow-up figures. Bottom lines go in the titles; the
# raw Inf/NA sweep stays in temp.

save_plot_pair <- function(p, dir, name) {
  ggsave(file.path(dir, paste0(name, ".png")), p,
    width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI
  )
  ggsave(file.path(dir, paste0(name, ".svg")), p,
    width = PLOT_WIDTH, height = PLOT_HEIGHT
  )
  invisible(TRUE)
}

# Published sweep: widths become "unbounded"/"unreliable" strings keyed to
# the three-state status (an unbounded set is reported as unbounded even when
# a finite side's validity certificate also failed); the curvature diagnostic
# is shown in scientific notation and only where computed (coarse grid).
published_sweep <- function(sweep) {
  data.frame(
    tau = sweep$tau,
    gamma = sweep$gamma,
    total_width = ifelse(
      sweep$status == "bounded",
      formatC(sweep$total_width, format = "f", digits = 4),
      sweep$status
    ),
    status = sweep$status,
    grid = sweep$grid,
    recession_normalized = ifelse(
      is.na(sweep$recession_normalized), "n/a",
      formatC(sweep$recession_normalized, format = "e", digits = 2)
    ),
    stringsAsFactors = FALSE
  )
}

# Width-vs-tau overlay across gamma choices. The bottom line -- how far
# optimization extends the slack tolerance -- is the title. Cap-censored tau*
# values render via .fmt_tau_star (">= cap"), and a censored optimizer tau*
# makes the extension factor a lower bound, never an exact claim.
plot_tau_star_overlay <- function(sweep, tau_stars) {
  df <- sweep[is.finite(sweep$total_width) & sweep$total_width > 0, ]
  ts <- stats::setNames(tau_stars$tau_star, tau_stars$gamma)
  capped <- stats::setNames(tau_stars$capped, tau_stars$gamma)
  ts_vfci <- ts[["VFCI (rank-1)"]]
  ts_opt <- ts[["optimized"]]
  opt_capped <- isTRUE(capped[["optimized"]])
  title <- if (is.finite(ts_opt) && is.finite(ts_vfci) && ts_vfci > 0) {
    sprintf(
      "Optimizing gamma extends slack tolerance %s%.0fx",
      if (opt_capped) ">= " else "~", ts_opt / ts_vfci
    )
  } else {
    "Identified-set width vs. slack tau"
  }
  ts_label <- paste(
    sprintf(
      "%s %s", names(ts),
      vapply(
        names(ts),
        function(g) .fmt_tau_star(ts[[g]], capped[[g]]), character(1)
      )
    ),
    collapse = ", "
  )
  p <- ggplot(df, aes(tau, total_width, color = gamma)) +
    geom_line() +
    geom_point(size = 1.3) +
    geom_vline(xintercept = ts_vfci, linetype = "dotted", color = "#2166AC") +
    scale_y_log10() +
    labs(
      title = title,
      subtitle = paste0(
        "tau* by gamma: ", ts_label, ".\n",
        "Verticals mark tau*; log scale, finite widths only."
      ),
      x = expression(tau), y = "Total profile-bound width (log, finite only)",
      color = "gamma"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  if (is.finite(ts_opt)) {
    p <- p + geom_vline(
      xintercept = ts_opt, linetype = "dashed", color = "#1B7837"
    )
  }
  rf_name <- grep("^reduced-form", names(ts), value = TRUE)[1]
  ts_rf <- if (!is.na(rf_name)) ts[[rf_name]] else NA_real_
  if (is.finite(ts_rf)) {
    p <- p + geom_vline(
      xintercept = ts_rf, linetype = "dotted", color = "#B2182B"
    )
  }
  p
}

# VFCI deep dive: how violently the identified set blows up as tau -> tau*.
# Zoomed to the bounded region plus a shaded unbounded band; fine-grid and
# bisection evaluations resolve the curve the coarse grid cannot.
plot_vfci_blowup <- function(sweep, tau_star) {
  vf <- sweep[sweep$gamma == "VFCI (rank-1)", ]
  xmax <- min(2 * tau_star, max(vf$tau))
  df <- vf[is.finite(vf$total_width) & vf$total_width > 0 & vf$tau <= xmax, ]
  title <- if (tau_star < 0.05) {
    sprintf(
      "VFCI baseline tolerates almost no slack: unbounded beyond tau* ~= %.5f",
      tau_star
    )
  } else {
    sprintf(
      "VFCI baseline: identified set unbounded beyond tau* ~= %.5f", tau_star
    )
  }
  ylim <- range(df$total_width) * c(0.8, 1.6)
  p <- ggplot(df, aes(tau, total_width))
  if (xmax > tau_star) {
    # finite band bounds (clipped by coord_cartesian below) keep the log
    # scale free of 0/Inf values
    p <- p +
      annotate("rect",
        xmin = tau_star, xmax = xmax, ymin = ylim[1] / 10, ymax = ylim[2] * 10,
        fill = "#B2182B", alpha = 0.08
      ) +
      annotate("text",
        x = (tau_star + xmax) / 2, y = max(df$total_width),
        label = "unbounded", color = "#B2182B", size = 3.5
      )
  }
  p <- p +
    coord_cartesian(ylim = ylim) +
    geom_line(color = "#2166AC") +
    geom_point(aes(shape = status), color = "#2166AC", size = 1.6) +
    scale_shape_manual(values = c(bounded = 16, unreliable = 4)) +
    geom_vline(xintercept = tau_star, linetype = "dashed", color = "#B2182B") +
    scale_y_log10() +
    labs(
      title = title,
      subtitle = paste0(
        "Total width explodes as tau -> tau* (dashed).\n",
        "tau = 0 is point-identified (width 0, not shown on the log scale)."
      ),
      x = expression(tau), y = "Total profile-bound width (log scale)",
      shape = "status"
    ) +
    theme_minimal()
  if (all(df$status == "bounded")) {
    p <- p + guides(shape = "none")
  }
  p
}
