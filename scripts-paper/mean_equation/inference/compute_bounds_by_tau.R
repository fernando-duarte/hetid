# Numerical coefficient ranges over the joint identified set on the slack grid.
# Coordinate and structural endpoints use the same shared refinement as the
# full sample and bootstrap. The theta tables also carry separate containing
# outer bounds for the volatility consumers; the plotted bands use endpoints.

paper_source_once(paper_path("support", "identification", "api.R"))
paper_source_once(paper_path("support", "identification", "profile_solver_core.R"))
paper_source_once(paper_path("support", "identification", "profile_bounds_api.R"))
paper_source_once(paper_path("support", "identification", "tau_star.R"))
paper_source_once(paper_path("support", "reporting", "inference.R"))
paper_source_once(paper_path("support", "graphics", "bounds_axis.R"))
paper_source_once(paper_path("mean_equation", "inference", "refine_bounds_by_tau.R"))
paper_source_once(paper_path("support", "graphics", "device.R"))
paper_source_once(paper_path("config", "tau_grid.R"))

theta_coefs <- set_id_mean_eq$theta_table$coef
beta_coefs <- set_id_mean_eq$beta1_table$coef

# warm-start state, seeded with the tau = 0 closed-form point (the whole set
# at tau = 0): each grid tau hands its accepted argmaxes to the next, where
# they are still feasible because the set grows with tau
seed_theta <- set_id_mean_eq$theta_table$point
if (anyNA(seed_theta)) seed_theta <- NULL
warm <- if (is.null(seed_theta)) list() else list(seed_theta)

# Retain the complete refined theta tables, including containing outer bounds,
# for downstream volatility searches and residual-zero screens.
mean_eq_bounds_tau <- list()
bounds_at_tau <- function(tau) {
  system <- mean_profile_system(set_id_mean_eq$gamma, tau, set_id_mean_eq$moments)
  it <- coef_interval_tables_widened(
    system$quadratic, set_id_mean_eq$beta1r, set_id_mean_eq$beta2r,
    points = system$points, warm = warm
  )
  warm <<- attr(it, "profile_points")
  tab <- rbind(it$beta1, it$theta[names(it$beta1)])
  mean_eq_bounds_tau[[paper_tau_key(tau)]] <<- it$theta
  data.frame(
    tau = tau, coef = tab$coef, lower = tab$set_lower, upper = tab$set_upper,
    certified = tab$status == PAPER_ENDPOINT_STATUS[["bounded"]]
  )
}

# rows recovered from the stored tables rather than re-solved: the tau = 0
# closed-form point (lower = upper) and the baseline-slack intervals, which
# estimate_identified_set.R already computed with the same solvers
tables <- rbind(set_id_mean_eq$beta1_table, set_id_mean_eq$theta_table)
stored_rows <- rbind(
  data.frame(
    tau = 0, coef = tables$coef, lower = tables$point, upper = tables$point,
    certified = !is.na(tables$point)
  ),
  data.frame(
    tau = set_id_mean_eq$tau_baseline, coef = tables$coef,
    lower = tables$set_lower, upper = tables$set_upper,
    certified = tables$status == PAPER_ENDPOINT_STATUS[["bounded"]]
  )
)

# solved tau grid, strictly inside (0, tau*): tau = 0 and the baseline come
# from the stored rows, and the tau* endpoint is excluded (the width diverges
# right at the transition, crushing every facet's scale)
tau_grid <- paper_bounds_tau_grid(set_id_mean_eq$tau_star)
bounds_df <- rbind(stored_rows, do.call(rbind, lapply(tau_grid, bounds_at_tau)))

# uncertified rows (unbounded or unreliable sides, expected near tau*) are
# dropped, truncating each coefficient's band where certification ends
plot_df <- bounds_df[bounds_df$certified, ]
plot_df$coef <- factor(plot_df$coef, levels = c(beta_coefs, theta_coefs))

ref_lines <- data.frame(
  tau = set_id_mean_eq$tau_baseline,
  line = sprintf(
    "baseline tau = %s",
    paper_format_tau(set_id_mean_eq$tau_baseline)
  )
)
figure_style <- PAPER_FIGURE_STYLE$identified_set
bounds_plot <- ggplot2::ggplot(plot_df, ggplot2::aes(tau)) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = lower, ymax = upper),
    fill = figure_style$primary,
    alpha = figure_style$ribbon_alpha
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = lower),
    color = figure_style$primary,
    linewidth = figure_style$boundary_linewidth
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = upper),
    color = figure_style$primary,
    linewidth = figure_style$boundary_linewidth
  ) +
  ggplot2::geom_vline(
    data = ref_lines, ggplot2::aes(xintercept = tau, linetype = line),
    color = figure_style$reference,
    linewidth = figure_style$reference_linewidth
  ) +
  ggplot2::facet_wrap(~coef, scales = "free_y", ncol = length(beta_coefs)) +
  ggplot2::labs(x = expression(tau), y = NULL, linetype = NULL) +
  # same display cap as the log-variance bounds panels, so the two exhibits are
  # cut at the same place in their own grids: a coord_cartesian zoom below the
  # branch switch near tau*, changing no tau, no bound and no tau*. The cap is
  # taken from the full sampled grid rather than the certified subset, because
  # certification can drop interior taus and leave gaps that are not the grid's
  # own subdivision.
  ggplot2::coord_cartesian(
    xlim = c(
      min(bounds_df$tau),
      paper_bounds_tau_display_cap(bounds_df$tau)
    )
  ) +
  ggplot2::theme(legend.position = "bottom")

device <- PAPER_FIGURE_RENDER_CONTROL$devices$mean_bounds
write_svg(
  artifact_path("mean_bounds_figure"),
  device[["width"]],
  device[["height"]],
  function() print(bounds_plot)
)

cat(
  "set-id bounds-by-tau figure:", length(unique(bounds_df$tau)),
  "tau values in [0,",
  paste0(
    signif(
      max(bounds_df$tau),
      PAPER_REPORTING_CONTROL$precision$figure_annotation
    ),
    "];"
  ),
  sum(!bounds_df$certified), "uncertified coefficient-tau rows dropped\n"
)

# warm-refined boxes at the display taus for the PPML set map: the estimator
# already refined these into set_tables with the same tau = 0-seeded warm chain,
# so they are re-keyed here rather than re-solved (the grid above is strictly
# inside (0, tau*) and never lands on a display tau)
mean_eq_bounds_tau[
  vapply(set_id_mean_eq$tau_display, paper_tau_key, character(1))
] <-
  lapply(set_id_mean_eq$set_tables, `[[`, "theta")

# widen_theta_box and solve_theta_bound_from stay: they belong to
# theta_box_multistart.R, which paper_source_once will not re-source, and the
# fitted-volatility tau sweep needs them for slacks off the display grid
rm(
  theta_coefs, beta_coefs, seed_theta, warm,
  bounds_at_tau, tables, stored_rows,
  tau_grid, bounds_df, plot_df, ref_lines, figure_style, bounds_plot, device
)
