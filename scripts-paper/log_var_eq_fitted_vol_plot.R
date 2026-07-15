# Rendering for estimator-agnostic date-indexed fitted-volatility envelopes.
# Unavailable dates split the ribbon and point curve into separate runs, so
# ggplot never bridges a fail-closed gap.

logvar_fitted_vol_run_id <- function(valid) {
  runs <- rle(valid)
  rep(seq_along(runs$lengths), runs$lengths)
}

logvar_fitted_vol_plot_data <- function(rows) {
  valid <- rows$lower_status == "bounded" & rows$upper_status == "bounded" &
    is.finite(rows$volatility_lower) & is.finite(rows$volatility_upper)
  band <- rows[valid, , drop = FALSE]
  band$run <- logvar_fitted_vol_run_id(valid)[valid]
  point_valid <- is.finite(rows$volatility_point)
  point <- rows[point_valid, , drop = FALSE]
  point$run <- logvar_fitted_vol_run_id(point_valid)[point_valid]
  list(band = band, point = point, n_omitted = sum(!valid))
}

logvar_fitted_vol_path <- function(out_dir, estimator) {
  file.path(
    out_dir,
    sprintf("log_var_eq_fitted_volatility_%s.pdf", estimator)
  )
}

logvar_fitted_vol_caption <- function(has_point) {
  point_note <- if (has_point) {
    "The red line is the tau = 0 Lewbel-point fit."
  } else {
    "The tau = 0 Lewbel-point fit is unavailable, so no red line is drawn."
  }
  paste0(
    "Shading is the pointwise projection hull of the estimated plug-in ",
    "variance-equation image.\n", point_note, " Finite plotted endpoints are ",
    "attained inner approximations\nfrom grid scan and local polish. This is ",
    "not a confidence or simultaneous path band; interior attainment is ",
    "not asserted."
  )
}

logvar_fitted_vol_render <- function(envelope, path) {
  rows <- envelope$data
  meta <- envelope$metadata
  plot_data <- logvar_fitted_vol_plot_data(rows)
  band <- plot_data$band
  point <- plot_data$point
  stopifnot(nrow(rows) > 0L)
  if (nrow(band) == 0L) {
    diag <- envelope$diagnostics$engine
    budget_note <- if (isTRUE(diag$budget_exhausted)) {
      paste0("; ", diag$budget_message)
    } else {
      ""
    }
    stop(sprintf(
      "%s fitted-volatility envelope has no two-sided bounded dates to plot%s",
      meta$estimator, budget_note
    ))
  }
  estimator_label <- switch(meta$estimator,
    ppml = "PPML",
    harvey = "Harvey",
    toupper(meta$estimator)
  )
  subtitle <- sprintf(
    "Pointwise envelope over the joint identified set at tau = %.2g",
    meta$tau
  )
  if (plot_data$n_omitted > 0L) {
    subtitle <- paste0(
      subtitle, "; ", plot_data$n_omitted,
      " unavailable date(s) omitted"
    )
  }
  fig <- ggplot2::ggplot(rows, ggplot2::aes(date)) +
    ggplot2::geom_ribbon(
      data = band,
      ggplot2::aes(
        ymin = volatility_lower, ymax = volatility_upper, group = run
      ),
      fill = "#2a78d6", alpha = 0.35
    ) +
    ggplot2::geom_line(
      data = band,
      ggplot2::aes(y = volatility_lower, group = run),
      color = "#2a78d6", linewidth = 0.4
    ) +
    ggplot2::geom_line(
      data = band,
      ggplot2::aes(y = volatility_upper, group = run),
      color = "#2a78d6", linewidth = 0.4
    ) +
    ggplot2::geom_line(
      data = point, ggplot2::aes(y = volatility_point, group = run),
      color = "#b2182b", linewidth = 0.55
    ) +
    ggplot2::labs(
      title = paste(estimator_label, "fitted conditional residual volatility"),
      subtitle = subtitle, x = NULL,
      y = "Conditional SD of consumption-growth residual (percentage points)",
      caption = logvar_fitted_vol_caption(nrow(point) > 0L)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(hjust = 0, size = 8),
      plot.margin = ggplot2::margin(8, 10, 8, 10)
    )
  built <- ggplot2::ggplot_build(fig)
  expected <- c(nrow(band), nrow(band), nrow(band), nrow(point))
  stopifnot(identical(vapply(built$data, nrow, integer(1)), expected))
  grDevices::pdf(path, width = 10, height = 6.1)
  print(fig)
  grDevices::dev.off()
  invisible(path)
}
