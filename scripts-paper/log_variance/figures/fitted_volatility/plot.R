# Rendering for estimator-agnostic date-indexed fitted-volatility envelopes.
# Unavailable dates split the ribbon and point curve into separate runs, so
# ggplot never bridges a fail-closed gap.

paper_source_once(paper_path("support", "graphics", "device.R"))
paper_source_once(paper_path("support", "reporting", "cells.R"))

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

logvar_fitted_vol_path <- function(estimator) {
  artifact_variant_path("fitted_volatility", estimator)
}

logvar_fitted_vol_band_stats <- function(band) {
  width <- band$volatility_upper - band$volatility_lower
  stopifnot(length(width) > 0L, all(is.finite(width)))
  point <- band$volatility_point
  relative <- width[is.finite(point) & point != 0] / abs(point[is.finite(point) &
    point != 0])
  imax <- which.max(width)
  list(
    median_width = stats::median(width),
    mean_width = mean(width),
    max_width = width[imax],
    max_qtr = as.character(band$qtr[imax]),
    median_relative = if (length(relative) > 0L) stats::median(relative) else NA_real_,
    max_relative = if (length(relative) > 0L) max(relative) else NA_real_
  )
}

logvar_fitted_vol_stats_note <- function(stats) {
  base <- sprintf(
    paste0(
      "Band width: median %.4f pp, mean %.4f pp, max %.4f pp at %s"
    ),
    stats$median_width, stats$mean_width, stats$max_width, stats$max_qtr
  )
  if (!is.finite(stats$median_relative) || !is.finite(stats$max_relative)) {
    return(paste0(base, "."))
  }
  sprintf(
    paste0("%s; relative to the red fit, median %.1f%% and max %.1f%%."),
    base, 100 * stats$median_relative, 100 * stats$max_relative
  )
}

logvar_fitted_vol_caption <- function(has_point, stats = NULL, polished = TRUE) {
  point_note <- if (has_point) {
    "The red line is the tau = 0 Lewbel-point fit."
  } else {
    "The tau = 0 Lewbel-point fit is unavailable, so no red line is drawn."
  }
  stats_note <- if (is.null(stats)) {
    ""
  } else {
    paste0("\n", logvar_fitted_vol_stats_note(stats))
  }
  # engine-profiled envelopes (grid scan then local polish) versus the grid-only
  # inner hull used for nonsmooth estimators, so the caption names how endpoints
  # were actually attained
  attainment <- if (polished) {
    "from grid scan and local polish"
  } else {
    "from a grid scan of the identified set"
  }
  paste0(
    "Shading is the pointwise projection hull of the estimated plug-in ",
    "variance-equation image.\n", point_note, " Finite plotted endpoints are ",
    "attained inner approximations\n", attainment, ". This is ",
    "not a confidence or simultaneous path band; interior attainment is ",
    "not asserted.", stats_note
  )
}

logvar_fitted_vol_render <- function(envelope, path) {
  figure_style <- PAPER_FIGURE_STYLE$identified_set
  logvar_style <- PAPER_FIGURE_STYLE$log_variance
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
  estimator_spec <- PAPER_LOGVAR_ESTIMATORS[[meta$estimator]]
  if (is.null(estimator_spec)) {
    estimator_spec <- list(
      response_scale = meta$response_scale,
      display_name = meta$estimator,
      display = list(
        title_quantity = "fitted conditional residual volatility",
        y_label = "Conditional residual volatility"
      )
    )
  }
  stopifnot(identical(
    meta$response_scale,
    estimator_spec$response_scale
  ))
  estimator_label <- estimator_spec$display_name
  title_quantity <- estimator_spec$display$title_quantity
  y_label <- estimator_spec$display$y_label
  subtitle <- sprintf(
    "Pointwise envelope over the joint identified set at tau = %s",
    paper_format_general(
      meta$tau,
      PAPER_REPORTING_CONTROL$precision$tau_significant
    )
  )
  if (plot_data$n_omitted > 0L) {
    subtitle <- paste0(
      subtitle, "; ", plot_data$n_omitted,
      " unavailable date(s) omitted"
    )
  }
  band_stats <- logvar_fitted_vol_band_stats(band)
  fig <- ggplot2::ggplot(rows, ggplot2::aes(date)) +
    ggplot2::geom_ribbon(
      data = band,
      ggplot2::aes(
        ymin = volatility_lower, ymax = volatility_upper, group = run
      ),
      fill = figure_style$primary,
      alpha = figure_style$ribbon_alpha
    ) +
    ggplot2::geom_line(
      data = band,
      ggplot2::aes(y = volatility_lower, group = run),
      color = figure_style$primary,
      linewidth = figure_style$boundary_linewidth
    ) +
    ggplot2::geom_line(
      data = band,
      ggplot2::aes(y = volatility_upper, group = run),
      color = figure_style$primary,
      linewidth = figure_style$boundary_linewidth
    ) +
    ggplot2::geom_line(
      data = point, ggplot2::aes(y = volatility_point, group = run),
      color = logvar_style$point,
      linewidth = logvar_style$point_linewidth
    ) +
    ggplot2::labs(
      title = paste(estimator_label, title_quantity),
      subtitle = subtitle, x = NULL,
      y = y_label,
      caption = logvar_fitted_vol_caption(
        nrow(point) > 0L, band_stats,
        polished = !isTRUE(meta$grid_only)
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(hjust = 0, size = 7.5),
      plot.margin = ggplot2::margin(8, 10, 8, 10)
    )
  built <- ggplot2::ggplot_build(fig)
  expected <- c(nrow(band), nrow(band), nrow(band), nrow(point))
  stopifnot(identical(vapply(built$data, nrow, integer(1)), expected))
  device <- PAPER_FIGURE_RENDER_CONTROL$devices$fitted_volatility
  write_svg(
    path,
    device[["width"]],
    device[["height"]],
    function() print(fig)
  )
}
