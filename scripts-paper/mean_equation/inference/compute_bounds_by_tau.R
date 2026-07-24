# Identified intervals for every coefficient of the structural consumption-
# growth equation as a function of the slack tau: on a grid over [0, tau*],
# the exact per-coefficient range of the joint identified set from
# estimate_identified_set.R (profile bounds for the news coefficients, linear-
# functional bounds beta1(theta) = beta1R - beta2R' theta for the design
# coefficients), with the closed-form tau = 0 point as the left endpoint.
# A warm-started refinement re-solves each news bound from the previous grid
# point's argmax: the shared solver starts every solve at the origin and can
# drop into a lower local vertex mid-grid (a dip in the pc2 upper bound near
# tau = 0.3), while continuation along the grid tracks the true branch.
# Writes the bounds SVG to the typed figure directory after mean-set estimation.

paper_source_once(paper_path("support", "identification", "api.R"))
paper_source_once(paper_path("support", "identification", "profile_solver_core.R"))
paper_source_once(paper_path("support", "identification", "profile_bounds_api.R"))
paper_source_once(paper_path("support", "identification", "tau_star.R"))
paper_source_once(paper_path("support", "reporting", "inference.R"))
paper_source_once(paper_path("mean_equation", "inference", "refine_bounds_by_tau.R"))
paper_source_once(paper_path("support", "graphics", "device.R"))

theta_coefs <- set_id_mean_eq$theta_table$coef
beta_coefs <- set_id_mean_eq$beta1_table$coef

# warm-start state, seeded with the tau = 0 closed-form point (the whole set
# at tau = 0): each successful solve hands its argmax to the next grid tau,
# where it is still feasible because the set grows with tau
seed_theta <- set_id_mean_eq$theta_table$point
if (anyNA(seed_theta)) seed_theta <- NULL
warm <- list(
  min = rep(list(seed_theta), length(theta_coefs)),
  max = rep(list(seed_theta), length(theta_coefs))
)
refined_n <- 0L

# warm-started refinement of the news intervals at one grid tau: re-solve
# each side from the previous grid point's argmax and keep a certified value
# only when it extends the origin-start solve
refine_theta_intervals <- function(tau, theta_tab) {
  qs <- tau_quadratic_system(set_id_mean_eq$gamma, tau, set_id_mean_eq$moments)
  for (k in seq_along(theta_coefs)) {
    for (side in c("min", "max")) {
      cand <- solve_theta_bound_from(qs, k, side, warm[[side]][[k]])
      if (is.null(cand)) next
      warm[[side]][[k]] <<- cand$theta
      if (theta_tab$status[k] != PAPER_ENDPOINT_STATUS[["bounded"]]) next
      if (side == "max" && cand$bound > theta_tab$set_upper[k]) {
        theta_tab$set_upper[k] <- cand$bound
        refined_n <<- refined_n + 1L
      } else if (side == "min" && cand$bound < theta_tab$set_lower[k]) {
        theta_tab$set_lower[k] <- cand$bound
        refined_n <<- refined_n + 1L
      }
    }
  }
  theta_tab
}

# per-coefficient interval of the joint identified set at one tau (the shared
# coef_interval_tables recipe); a row is certified only when both sides are
# finite and feasibility-valid, i.e. status "bounded". The refined theta
# tables are kept per tau (full three-state status) as mean_eq_bounds_tau
# for the log-variance bounds-by-tau figure, whose census needs these exact
# warm-refined boxes as its sound outer screen
mean_eq_bounds_tau <- list()
bounds_at_tau <- function(tau) {
  it <- coef_interval_tables(
    set_id_mean_eq$gamma, tau, set_id_mean_eq$moments,
    set_id_mean_eq$beta1r, set_id_mean_eq$beta2r
  )
  it$theta <- refine_theta_intervals(tau, it$theta)
  mean_eq_bounds_tau[[paper_tau_key(tau)]] <<- it$theta
  tab <- rbind(it$beta1, it$theta)
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
tau_grid <- seq(
  0,
  set_id_mean_eq$tau_star,
  length.out = PAPER_ANALYSIS_CONTRACT$tau$figure_grid_n
)
tau_grid <- tau_grid[tau_grid > 0 & tau_grid < set_id_mean_eq$tau_star]
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
  sum(!bounds_df$certified), "uncertified coefficient-tau rows dropped;",
  refined_n, "sides extended by warm-start refinement\n"
)

# warm-refined boxes at the display taus for the PPML set map: the estimator
# already refined these into set_tables with the same tau = 0-seeded warm chain,
# so they are re-keyed here rather than re-solved (the grid above is strictly
# inside (0, tau*) and never lands on a display tau)
mean_eq_bounds_tau[
  vapply(set_id_mean_eq$tau_display, paper_tau_key, character(1))
] <-
  lapply(set_id_mean_eq$set_tables, `[[`, "theta")

rm(
  theta_coefs, beta_coefs, solve_theta_bound_from, seed_theta, warm,
  refined_n, refine_theta_intervals, bounds_at_tau, tables, stored_rows,
  tau_grid, bounds_df, plot_df, ref_lines, figure_style, bounds_plot, device
)
