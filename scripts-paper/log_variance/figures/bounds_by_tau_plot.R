# Rendering for the log-variance bounds-by-tau figure: ribbons where both
# sides are certified, one-sided finite endpoints with divergence markers,
# and a per-facet status strip placed data-driven below each facet's data
# (never a y = -Inf hack). The plotting frame is asserted before rendering
# and the built layers are asserted after (a layer silently dropping to zero
# rows fails the run). Definitions only; sourced by render_bounds_by_tau.R.

paper_source_once(paper_path("support", "graphics", "device.R"))

logvar_bounds_tau_render <- function(rows, metadata, tau_baseline, tau_star,
                                     path) {
  figure_style <- PAPER_FIGURE_STYLE$identified_set
  logvar_style <- PAPER_FIGURE_STYLE$log_variance
  rows$category <- ifelse(
    rows$lower_status == "bounded" & rows$upper_status == "bounded",
    "two-sided",
    ifelse(
      (rows$lower_status == "bounded" & rows$upper_status == "unbounded") |
        (rows$upper_status == "bounded" & rows$lower_status == "unbounded"),
      "one-sided",
      ifelse(
        rows$lower_status == "unbounded" & rows$upper_status == "unbounded",
        "unbounded", "unreliable"
      )
    )
  )
  rows$finite_side <- ifelse(
    rows$category == "one-sided",
    ifelse(rows$lower_status == "bounded", rows$lower, rows$upper),
    NA_real_
  )
  rows$direction <- ifelse(
    rows$category == "one-sided",
    ifelse(rows$upper_status == "unbounded", "up", "down"),
    NA_character_
  )
  rows$coef <- factor(rows$coef, levels = unique(rows$coef))
  two <- rows[rows$category == "two-sided", ]
  one <- rows[rows$category == "one-sided", ]
  n_tally <- table(factor(
    rows$category,
    levels = c("two-sided", "one-sided", "unbounded", "unreliable")
  ))
  cat(
    "  bounds-by-tau rows: two-sided", n_tally[["two-sided"]],
    "one-sided", n_tally[["one-sided"]],
    "unbounded", n_tally[["unbounded"]],
    "unreliable", n_tally[["unreliable"]], "\n"
  )
  stopifnot(
    nrow(rows) > 0L,
    all(is.finite(two$lower)), all(is.finite(two$upper)),
    all(is.finite(one$finite_side)), !anyNA(one$direction)
  )
  # per-facet strip placement below the facet's own data, with a floor so a
  # point-collapsed facet still separates strip from data
  strip <- do.call(rbind, lapply(levels(rows$coef), function(cf) {
    sub <- rows[rows$coef == cf, ]
    vals <- c(
      sub$lower[is.finite(sub$lower)], sub$upper[is.finite(sub$upper)],
      sub$finite_side[is.finite(sub$finite_side)]
    )
    ymin <- if (length(vals)) min(vals) else 0
    rng <- max(1e-4, if (length(vals)) diff(range(vals)) else 0)
    data.frame(
      coef = cf, tau = sub$tau, category = sub$category,
      y = ymin - 0.06 * rng, h = 0.04 * rng
    )
  }))
  strip$coef <- factor(strip$coef, levels = levels(rows$coef))
  ref_line <- data.frame(
    tau = tau_baseline,
    line = sprintf(
      "baseline tau = %s",
      paper_format_general(
        tau_baseline,
        PAPER_REPORTING_CONTROL$precision$tau_significant
      )
    )
  )
  fig <- ggplot2::ggplot(rows, ggplot2::aes(tau)) +
    ggplot2::geom_ribbon(
      data = two, ggplot2::aes(ymin = lower, ymax = upper),
      fill = figure_style$primary,
      alpha = figure_style$ribbon_alpha
    ) +
    ggplot2::geom_line(
      data = two, ggplot2::aes(y = lower),
      color = figure_style$primary,
      linewidth = figure_style$boundary_linewidth
    ) +
    ggplot2::geom_line(
      data = two, ggplot2::aes(y = upper),
      color = figure_style$primary,
      linewidth = figure_style$boundary_linewidth
    ) +
    ggplot2::geom_line(
      data = one, ggplot2::aes(y = finite_side),
      color = logvar_style$one_sided,
      linewidth = figure_style$boundary_linewidth
    ) +
    ggplot2::geom_point(
      data = one, ggplot2::aes(y = finite_side, shape = direction),
      color = logvar_style$one_sided,
      fill = NA,
      size = logvar_style$one_sided_point_size
    ) +
    ggplot2::geom_tile(
      data = strip,
      ggplot2::aes(y = y, height = h, fill = category),
      width = tau_star / 30
    ) +
    ggplot2::geom_vline(
      data = ref_line, ggplot2::aes(xintercept = tau, linetype = line),
      color = figure_style$reference,
      linewidth = figure_style$reference_linewidth
    ) +
    ggplot2::scale_fill_manual(values = c(
      "two-sided" = figure_style$primary,
      "one-sided" = logvar_style$one_sided,
      "unbounded" = logvar_style$unbounded,
      "unreliable" = logvar_style$unreliable
    )) +
    ggplot2::facet_wrap(~coef, scales = "free_y", ncol = 3) +
    ggplot2::labs(
      x = expression(tau), y = NULL, linetype = NULL, shape = NULL,
      fill = "status",
      caption = paste0(
        "Estimator ", metadata$estimator, "; target functional ",
        metadata$target_functional, ". Bands are projection hulls of an ",
        "estimated plug-in image; interior attainment is not established; ",
        "finite endpoints are inner approximations. tau* = ",
        signif(
          tau_star,
          PAPER_REPORTING_CONTROL$precision$figure_annotation
        ),
        " is the mean-equation set's ",
        "bounded-unbounded transition and is excluded from the grid."
      )
    ) +
    ggplot2::theme(legend.position = "bottom")
  # the divergence-marker scale has nothing to match on a map where no side
  # diverges (the median map, and the variance maps at these taus): a manual
  # scale over an empty aesthetic warns on every build and is then dropped, so
  # add it only when the one-sided layer actually carries rows
  if (nrow(one) > 0L) {
    fig <- fig + ggplot2::scale_shape_manual(
      values = c(up = 24, down = 25),
      labels = c(up = "upper side diverges", down = "lower side diverges")
    )
  }
  built <- ggplot2::ggplot_build(fig)
  layer_rows <- vapply(built$data, nrow, integer(1))
  # the reference line's one data row is replicated into every facet panel
  expected <- c(
    nrow(two), nrow(two), nrow(two), nrow(one), nrow(one), nrow(strip),
    nlevels(rows$coef)
  )
  stopifnot(identical(layer_rows, expected))
  device <- PAPER_FIGURE_RENDER_CONTROL$devices$logvar_bounds
  write_svg(
    path,
    device[["width"]],
    device[["height"]],
    function() print(fig)
  )
}
