# Bounds-by-tau figure for the log-variance equation: per-coefficient
# projection-hull bands of theta_hat over the joint b_N set as functions of
# the slack tau, computed through the shared engine with a warm chain and a
# cross-tau cache. The b_N boxes come from the mean-equation figure's
# warm-refined intervals (mean_eq_bounds_tau, set_id_bounds_tau.R) so both
# figures describe the same sets and the census keeps a sound outer screen;
# the four display-tau rows are lifted verbatim from log_var_eq$schema
# (frozen benchmark numbers, computed from the driver's raw boxes -- a
# disclosed asymmetry, not a recomputation trigger). Bands are projection
# hulls of an estimated plug-in image; interior attainment is not
# established. Run via run_all.R after set_id_bounds_tau.R.

source("scripts-paper/log_var_eq_bounds_tau_plot.R")

# the frozen estimator, seed, and sample guard; assert the stored ids agree
# and match a fresh recomputation from the same qtr-joined sample
fig_est <- log_var_eq$estimator
fig_seed <- log_var_eq$b_seed
fig_rows <- dplyr::inner_join(
  tibble::tibble(qtr = set_id_mean_eq$qtr, row = seq_along(set_id_mean_eq$qtr)),
  lag_asset_return_pc,
  by = "qtr"
) |>
  dplyr::arrange(qtr)
fig_pcr <- scale(
  as.matrix(fig_rows[value_cols(lag_asset_return_pc)]),
  center = TRUE, scale = FALSE
)
stopifnot(
  identical(log_var_eq$sample_id, fig_est$metadata$sample_id),
  identical(log_var_eq$sample_id, logvar_sample_id(
    fig_rows$qtr, set_id_mean_eq$w1[fig_rows$row],
    set_id_mean_eq$w2[fig_rows$row, , drop = FALSE], fig_pcr
  ))
)

# engine runs on the same tau grid the mean-equation figure solved, using
# its refined b_N boxes; one cache and one budget state span the grid, and
# each tau's certified arg endpoints seed the next tau's extra starts
fig_tau_grid <- seq(0, set_id_mean_eq$tau_star, length.out = 25)
fig_tau_grid <- fig_tau_grid[fig_tau_grid > 0 & fig_tau_grid < set_id_mean_eq$tau_star]
fig_cache <- new.env(parent = emptyenv())
fig_bs <- logvar_budget_state()
fig_run <- function(tau, extra) {
  b_tab <- mean_eq_bounds_tau[[sprintf("%.17g", tau)]]
  stopifnot(!is.null(b_tab))
  qs <- tau_quadratic_system(set_id_mean_eq$gamma, tau, set_id_mean_eq$moments)
  logvar_engine_set_at_tau(
    fig_est, qs, b_tab,
    b_seed = fig_seed, extra_starts = extra,
    cache = fig_cache, budget_state = fig_bs, tau = tau
  )
}
fig_args <- function(s) {
  a <- c(s$arg_lower[s$lower_status == "bounded"], s$arg_upper[s$upper_status == "bounded"])
  a[!vapply(a, anyNA, logical(1))]
}
fig_res <- list()
fig_warm <- NULL
for (fig_tau in fig_tau_grid) {
  res <- fig_run(fig_tau, fig_warm)
  fig_res[[sprintf("%.17g", fig_tau)]] <- res
  fig_warm <- fig_args(res$schema)
}

# nesting among the engine-computed grid rows only (display rows are frozen
# under fewer starts and can trigger no downgrade): one warm retry per
# violating tau from the neighboring taus' args, then the narrower claim is
# downgraded to unreliable with a console line per violation
fig_grid_rows <- function() {
  do.call(rbind, lapply(fig_res, function(r) {
    r$schema[c("tau", "coef", "lower", "upper", "lower_status", "upper_status")]
  }))
}
fig_viol <- logvar_check_nesting(fig_grid_rows())
if (nrow(fig_viol) > 0L) {
  for (tv in unique(fig_viol$tau)) {
    k <- match(sprintf("%.17g", tv), names(fig_res))
    near <- c(
      if (k > 1L) fig_args(fig_res[[k - 1L]]$schema),
      fig_args(fig_res[[k]]$schema),
      if (k < length(fig_res)) fig_args(fig_res[[k + 1L]]$schema)
    )
    fig_res[[k]] <- fig_run(tv, near)
  }
  fig_viol <- logvar_check_nesting(fig_grid_rows())
  for (i in seq_len(nrow(fig_viol))) {
    v <- fig_viol[i, ]
    k <- match(sprintf("%.17g", v$tau), names(fig_res))
    col <- paste0(v$side, "_status")
    j <- match(v$coef, fig_res[[k]]$schema$coef)
    fig_res[[k]]$schema[[col]][j] <- "unreliable"
    cat(sprintf(
      "  nesting violation retained: %s %s side at tau = %.4g (gap %.3g); downgraded\n",
      v$coef, v$side, v$tau, v$violation
    ))
  }
}

# assemble the plotting rows: engine grid rows, frozen display rows, and the
# tau = 0 Lewbel-point value (lower = upper, certified iff finite)
fig_pick <- c("tau", "coef", "lower", "upper", "lower_status", "upper_status")
fig_plot_rows <- rbind(
  cbind(fig_grid_rows(), source = "grid"),
  cbind(do.call(rbind, lapply(log_var_eq$schema, `[`, fig_pick)), source = "display")
)
if (!anyNA(fig_seed)) {
  fig_point <- fig_est$fit_at_b(fig_seed)$coef
  fig_plot_rows <- rbind(fig_plot_rows, data.frame(
    tau = 0, coef = names(fig_point), lower = unname(fig_point),
    upper = unname(fig_point),
    lower_status = ifelse(is.finite(fig_point), "bounded", "unreliable"),
    upper_status = ifelse(is.finite(fig_point), "bounded", "unreliable"),
    source = "point", row.names = NULL
  ))
}
row.names(fig_plot_rows) <- NULL

fig_path <- logvar_bounds_tau_path(out_dir, fig_est$metadata)
logvar_bounds_tau_render(
  fig_plot_rows, fig_est$metadata, set_id_mean_eq$tau_baseline,
  set_id_mean_eq$tau_star, fig_path
)

cat(
  "log-variance bounds-by-tau figure (", fig_est$metadata$estimator, "): ",
  length(fig_tau_grid), " grid taus; crossings ",
  paste(vapply(fig_res, function(r) r$n_cross, integer(1)), collapse = " "),
  "; cache hits ", fig_bs$counters[["cache_hit"]],
  "; nesting downgrades ", nrow(fig_viol), "\n",
  sep = ""
)

rm(
  fig_est, fig_seed, fig_rows, fig_pcr, fig_tau_grid, fig_cache, fig_bs,
  fig_run, fig_args, fig_res, fig_warm, fig_grid_rows, fig_viol, fig_pick,
  fig_plot_rows, fig_path, fig_tau
)
