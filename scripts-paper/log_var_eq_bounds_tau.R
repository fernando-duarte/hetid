# Bounds-by-tau figures for the log-variance equation, one estimator-stamped
# PDF per logvar_bounds_tau_registry entry (entry one is the benchmark
# log-OLS map, whose output is the regression contract). Per entry: engine
# runs over the mean-equation figure's warm-refined b_N boxes
# (mean_eq_bounds_tau, set_id_bounds_tau.R) with a warm chain and a
# cross-tau cache; display-tau rows are lifted verbatim from the entry's
# frozen schema; a nesting check with one warm retry downgrades the narrower
# claim. Bands are projection hulls of an estimated plug-in image; interior
# attainment is not established. Run via run_all.R after set_id_bounds_tau.R.

source("scripts-paper/log_var_eq_bounds_tau_plot.R")

# fresh sample-id recomputation from the same qtr-joined sample; every
# registry estimator must carry this id
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
fig_fresh_id <- logvar_sample_id(
  fig_rows$qtr, set_id_mean_eq$w1[fig_rows$row],
  set_id_mean_eq$w2[fig_rows$row, , drop = FALSE], fig_pcr
)
stopifnot(
  identical(log_var_eq$sample_id, log_var_eq$estimator$metadata$sample_id),
  identical(log_var_eq$sample_id, fig_fresh_id)
)

fig_tau_grid <- seq(0, set_id_mean_eq$tau_star, length.out = 25)
fig_tau_grid <- fig_tau_grid[fig_tau_grid > 0 & fig_tau_grid < set_id_mean_eq$tau_star]
fig_args <- function(s) {
  a <- c(s$arg_lower[s$lower_status == "bounded"], s$arg_upper[s$upper_status == "bounded"])
  a[!vapply(a, anyNA, logical(1))]
}

# one figure per registry entry: engine grid walk, nesting guard with a warm
# retry and disclosed downgrades, plot assembly, and the render
logvar_bounds_tau_entry <- function(entry) {
  est <- entry$estimator
  stopifnot(identical(est$metadata$sample_id, fig_fresh_id))
  opts <- entry$engine_opts
  if (is.null(opts$cache)) opts$cache <- new.env(parent = emptyenv())
  if (is.null(opts$budget_state)) opts$budget_state <- logvar_budget_state()
  run_tau <- function(tau, extra) {
    b_tab <- mean_eq_bounds_tau[[sprintf("%.17g", tau)]]
    stopifnot(!is.null(b_tab))
    qs <- tau_quadratic_system(set_id_mean_eq$gamma, tau, set_id_mean_eq$moments)
    do.call(logvar_engine_set_at_tau, c(
      list(est, qs, b_tab, b_seed = entry$b_seed, extra_starts = extra, tau = tau),
      opts
    ))
  }
  res <- list()
  warm <- NULL
  for (tau in fig_tau_grid) {
    r <- run_tau(tau, warm)
    res[[sprintf("%.17g", tau)]] <- r
    warm <- fig_args(r$schema)
  }
  grid_rows <- function() {
    do.call(rbind, lapply(res, function(r) {
      r$schema[c("tau", "coef", "lower", "upper", "lower_status", "upper_status")]
    }))
  }
  viol <- logvar_check_nesting(grid_rows())
  if (nrow(viol) > 0L) {
    for (tv in unique(viol$tau)) {
      k <- match(sprintf("%.17g", tv), names(res))
      near <- c(
        if (k > 1L) fig_args(res[[k - 1L]]$schema),
        fig_args(res[[k]]$schema),
        if (k < length(res)) fig_args(res[[k + 1L]]$schema)
      )
      res[[k]] <- run_tau(tv, near)
    }
    viol <- logvar_check_nesting(grid_rows())
    for (i in seq_len(nrow(viol))) {
      v <- viol[i, ]
      k <- match(sprintf("%.17g", v$tau), names(res))
      col <- paste0(v$side, "_status")
      j <- match(v$coef, res[[k]]$schema$coef)
      res[[k]]$schema[[col]][j] <- "unreliable"
      cat(sprintf(
        "  nesting violation retained: %s %s side at tau = %.4g (gap %.3g); downgraded\n",
        v$coef, v$side, v$tau, v$violation
      ))
    }
  }
  pick <- c("tau", "coef", "lower", "upper", "lower_status", "upper_status")
  plot_rows <- rbind(
    cbind(grid_rows(), source = "grid"),
    cbind(do.call(rbind, lapply(entry$schema, `[`, pick)), source = "display")
  )
  if (!is.null(entry$b_seed) && !anyNA(entry$b_seed)) {
    pt <- est$fit_at_b(entry$b_seed)$coef
    plot_rows <- rbind(plot_rows, data.frame(
      tau = 0, coef = names(pt), lower = unname(pt), upper = unname(pt),
      lower_status = ifelse(is.finite(pt), "bounded", "unreliable"),
      upper_status = ifelse(is.finite(pt), "bounded", "unreliable"),
      source = "point", row.names = NULL
    ))
  }
  row.names(plot_rows) <- NULL
  logvar_bounds_tau_render(
    plot_rows, est$metadata, set_id_mean_eq$tau_baseline,
    set_id_mean_eq$tau_star, entry$output_path
  )
  cat(
    "log-variance bounds-by-tau figure (", est$metadata$estimator, "): ",
    length(fig_tau_grid), " grid taus; crossings ",
    paste(vapply(res, function(r) r$n_cross, integer(1)), collapse = " "),
    "; cache hits ", opts$budget_state$counters[["cache_hit"]],
    "; nesting downgrades ", nrow(viol), "\n",
    sep = ""
  )
}

for (fig_entry in logvar_bounds_tau_registry) logvar_bounds_tau_entry(fig_entry)

rm(
  fig_rows, fig_pcr, fig_fresh_id, fig_tau_grid, fig_args,
  logvar_bounds_tau_entry, fig_entry
)
