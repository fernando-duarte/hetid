# Three-way identification-strength comparison via tau* (the slack at which the
# identified set goes bounded -> unbounded), for three gamma choices:
#   VFCI (rank-1 baseline), reduced-form (rank-3 benchmark), optimized.
# tau* is scale-free and needs no cherry-picked tau, so it is the honest
# quantitative summary of "what optimization buys" (it EXTENDS tau*).
# NOTE: each A_i = Q_iQ_i' - d_i S_i^(2) has at most one positive eigenvalue
# (rank-1 PSD minus d_i>=0 times a Gram/PSD matrix); boundedness is the JOINT
# recession condition that the positive-curvature directions cover R^I, a
# (gamma, tau) property -- not implied by rank(gamma).

source(here::here("scripts/utils/common_settings.R"))
load_visualization_packages()

cli_h1("Identification strength: tau* across gamma choices")

inp <- load_identification_inputs(mode = "factors")
resid <- compute_identification_residuals(inp$data, mode = "factors")
moments <- compute_identification_moments(resid$w1, resid$w2, resid$pcs_aligned)
n_comp <- nrow(inp$lookup)
gamma_vfci <- get_baseline_gamma("vfci", n_components = n_comp)
gamma_rf <- resid$gamma_rf

bounded_at <- function(gamma, tau) {
  b <- solve_all_profile_bounds(symmetrize_quadratic_system(
    build_quadratic_system(gamma, rep(tau, n_comp), moments)
  )$quadratic)
  list(
    total = sum(b$width),
    bounded = all(b$bounded_lower & b$bounded_upper)
  )
}

# tau* for a FIXED gamma: bisect the bounded -> unbounded transition.
tau_star_fixed <- function(gamma, grid = seq(0.005, 0.2, by = 0.005)) {
  bnd <- vapply(grid, function(t) bounded_at(gamma, t)$bounded, logical(1))
  if (all(bnd)) {
    return(max(grid))
  }
  hi <- grid[which(!bnd)[1]]
  lo <- if (any(bnd & grid < hi)) max(grid[bnd & grid < hi]) else 0
  for (k in seq_len(40)) {
    mid <- (lo + hi) / 2
    if (bounded_at(gamma, mid)$bounded) lo <- mid else hi <- mid
  }
  (lo + hi) / 2
}

# tau* for the OPTIMIZER: largest tau where re-optimizing yields a bounded+valid
# set (objective_final finite). Bracket upward from 0.2, then bisect.
opt_bounded <- function(tau) {
  is.finite(run_gamma_optimization(
    gamma_vfci, moments, rep(tau, n_comp),
    n_starts = 15, seed = SEED
  )$objective_final)
}

cli_h2("tau* (fixed gammas)")
ts_vfci <- tau_star_fixed(gamma_vfci)
ts_rf <- tau_star_fixed(gamma_rf)
cli_alert_info("tau*(VFCI, rank {qr(gamma_vfci)$rank}) = {.val {round(ts_vfci, 4)}}")
cli_alert_info("tau*(reduced-form, rank {qr(gamma_rf)$rank}) = {.val {round(ts_rf, 4)}}")

cli_h2("tau* (optimizer; bracket + bisection)")
lo <- 0.2
hi <- 0.4
ts_opt <- NA_real_
if (!opt_bounded(lo)) {
  cli_alert_warning("Optimizer not bounded even at tau=0.2; tau*(opt) < 0.2")
} else {
  while (opt_bounded(hi) && hi < 5) {
    lo <- hi
    hi <- hi * 2
  }
  if (opt_bounded(hi)) {
    ts_opt <- hi
    cli_alert_info("tau*(opt) >= {.val {hi}} (search cap)")
  } else {
    for (k in seq_len(25)) {
      mid <- (lo + hi) / 2
      if (opt_bounded(mid)) lo <- mid else hi <- mid
    }
    ts_opt <- (lo + hi) / 2
  }
}
cli_alert_success("tau*(optimized) = {.val {round(ts_opt, 4)}}")
cli_alert_success(
  "Optimization extends tau* by ~{.val {round(ts_opt / ts_vfci, 1)}}x vs VFCI"
)

# width-vs-tau overlay for the two fixed gammas
grid <- seq(0, 0.2, by = 0.005)
sweep_one <- function(gamma, label) {
  do.call(rbind, lapply(grid, function(t) {
    if (t == 0) {
      return(data.frame(tau = 0, total_width = 0, all_bounded = TRUE, gamma = label))
    }
    w <- bounded_at(gamma, t)
    data.frame(tau = t, total_width = w$total, all_bounded = w$bounded, gamma = label)
  }))
}
sweep_df <- rbind(
  sweep_one(gamma_vfci, "VFCI (rank-1)"),
  sweep_one(gamma_rf, "reduced-form (rank-3)")
)

out_dir <- file.path(OUTPUT_PAPER_DIR, "identification")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
summary_df <- data.frame(
  gamma = c("VFCI (rank-1)", "reduced-form (rank-3)", "optimized"),
  tau_star = c(ts_vfci, ts_rf, ts_opt)
)
write.csv(summary_df, file.path(out_dir, "tau_star_comparison.csv"), row.names = FALSE)
write.csv(sweep_df, file.path(out_dir, "tau_star_width_sweep.csv"), row.names = FALSE)

p <- ggplot(
  sweep_df[is.finite(sweep_df$total_width), ],
  aes(tau, total_width, color = gamma)
) +
  geom_line() +
  geom_point(size = 1.3) +
  geom_vline(xintercept = ts_vfci, linetype = "dotted", color = "#2166AC") +
  geom_vline(xintercept = ts_rf, linetype = "dotted", color = "#B2182B") +
  geom_vline(xintercept = ts_opt, linetype = "dashed", color = "#1B7837") +
  scale_y_log10() +
  labs(
    title = "Identification strength: identified-set width vs. slack tau",
    subtitle = sprintf(
      "tau*: VFCI %.3f, reduced-form %.3f, optimized %.3f (green dashed)",
      ts_vfci, ts_rf, ts_opt
    ),
    x = expression(tau), y = "Total profile-bound width (log, finite only)",
    color = "gamma"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave(file.path(out_dir, "tau_star_comparison.png"), p,
  width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI
)
ggsave(file.path(out_dir, "tau_star_comparison.svg"), p,
  width = PLOT_WIDTH, height = PLOT_HEIGHT
)

cli_h2("Summary")
print(summary_df, digits = 4)
cli_alert_success("Saved tau_star_comparison CSV/PNG/SVG to {.path {out_dir}}")
