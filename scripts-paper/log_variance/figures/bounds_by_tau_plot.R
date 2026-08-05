# Rendering for the log-variance bounds-by-tau figure: ribbons where both
# sides are certified, one-sided finite endpoints with divergence markers,
# and a per-facet status strip placed data-driven below each facet's data
# (never a y = -Inf hack). The plotting frame is asserted before rendering
# and the built layers are asserted after (a layer silently dropping to zero
# rows fails the run). Definitions only; sourced by render_bounds_by_tau.R.

paper_source_once(paper_path("support", "graphics", "device.R"))
paper_source_once(paper_path("support", "graphics", "bounds_axis.R"))
paper_source_once(paper_path("log_variance", "figures", "bounds_by_tau_frame.R"))

logvar_bounds_tau_render <- function(rows, metadata, tau_baseline, tau_star,
                                     path) {
  figure_style <- PAPER_FIGURE_STYLE$identified_set
  logvar_style <- PAPER_FIGURE_STYLE$log_variance
  # per-facet strip placement below the facet's own data, with a floor so a
  # point-collapsed facet still separates strip from data
  frame <- logvar_bounds_tau_frame(rows, tau_star)
  rows <- frame$rows
  two <- frame$two
  one <- frame$one
  strip <- frame$strip
  # the slack range past the largest sampled tau: never solved, so it is drawn
  # as an explicitly uncharacterised band rather than left as blank axis
  unsampled <- data.frame(lo = max(rows$tau), hi = tau_star)
  # Display cap: the last sampled tau below the branch-switch kink. Every row
  # stays in the data and in every assertion below; the coordinate system just
  # zooms, so nothing here changes a bound, a grid or tau*.
  sampled_taus <- sort(unique(rows$tau))
  x_cap <- paper_bounds_tau_display_cap(sampled_taus)
  n_hidden <- sum(sampled_taus > x_cap)
  # the uncharacterised band sits above the largest sampled tau, so once the axis
  # is capped below it the caption must stop advertising it
  band_note <- if (n_hidden == 0L) {
    paste0(
      " The shaded band above the largest sampled tolerance is not ",
      "characterized."
    )
  } else {
    sprintf(
      paste0(
        " The axis is truncated at tau = %s, the last sampled tolerance below ",
        "the branch switch near tau*; %d sampled tolerances above it are ",
        "computed and reported but not drawn."
      ),
      signif(x_cap, PAPER_REPORTING_CONTROL$precision$figure_annotation),
      n_hidden
    )
  }
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
    ggplot2::geom_rect(
      data = unsampled, inherit.aes = FALSE,
      ggplot2::aes(xmin = lo, xmax = hi, ymin = -Inf, ymax = Inf),
      fill = logvar_style$unbounded, alpha = logvar_style$unsampled_shade
    ) +
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
    # sampled slacks on the certified boundaries: the segments between them are
    # interpolation, not a claim about the shape in between
    ggplot2::geom_point(
      data = two, ggplot2::aes(y = lower),
      color = figure_style$primary, size = logvar_style$sampled_point_size
    ) +
    ggplot2::geom_point(
      data = two, ggplot2::aes(y = upper),
      color = figure_style$primary, size = logvar_style$sampled_point_size
    ) +
    ggplot2::geom_tile(
      data = strip,
      ggplot2::aes(y = y, height = h, width = w, fill = category)
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
        "bounded-unbounded transition. Points mark the sampled tolerances and ",
        "the segments between them are interpolation.", band_note
      )
    ) +
    ggplot2::coord_cartesian(xlim = c(min(sampled_taus), x_cap)) +
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
  # the unsampled band's and the reference line's one data row are each
  # replicated into every facet panel
  n_facet <- nlevels(rows$coef)
  expected <- c(
    n_facet, nrow(two), nrow(two), nrow(two), nrow(one), nrow(one),
    nrow(two), nrow(two), nrow(strip), n_facet
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
