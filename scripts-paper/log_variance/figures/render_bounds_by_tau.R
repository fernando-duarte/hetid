# Bounds-by-tau figures for the log-variance equation, one estimator-stamped
# SVG per logvar_bounds_tau_registry entry (entry one is the benchmark
# log-OLS map, whose output is the regression contract). Per entry: engine
# runs over the mean-equation figure's warm-refined b_N boxes
# (mean_eq_bounds_tau, compute_bounds_by_tau.R) with a warm chain and a
# cross-tau cache; display-tau rows are lifted verbatim from the entry's
# frozen schema; a nesting check with one warm retry downgrades the narrower
# claim. Bands are projection hulls of an estimated plug-in image; interior
# attainment is not established. Run via run_pipeline.R after compute_bounds_by_tau.R.

paper_source_once(paper_path("log_variance", "figures", "bounds_by_tau_plot.R"))
paper_source_once(paper_path("config", "tau_grid.R"))

# fresh sample-id recomputation from the same qtr-joined sample; every
# registry estimator must carry this id
fig_rows <- dplyr::inner_join(
  tibble::tibble(qtr = set_id_mean_eq$qtr, row = seq_along(set_id_mean_eq$qtr)),
  lag_asset_return_pc,
  by = PAPER_ANALYSIS_CONTRACT$model$key_col
) |>
  dplyr::arrange(qtr)
fig_pcr <- paper_normalize_model_matrix(
  as.matrix(fig_rows[value_cols(lag_asset_return_pc)]),
  PAPER_ANALYSIS_CONTRACT$model$preprocessing$return_pc
)
fig_fresh_id <- logvar_sample_id(
  fig_rows$qtr, set_id_mean_eq$w1[fig_rows$row],
  set_id_mean_eq$w2[fig_rows$row, , drop = FALSE], fig_pcr
)
stopifnot(
  identical(log_var_eq$sample_id, log_var_eq$estimator$metadata$sample_id),
  identical(log_var_eq$sample_id, fig_fresh_id)
)

fig_tau_grid <- paper_bounds_tau_grid(set_id_mean_eq$tau_star)

# a raw feasible count below the search floor means the lattice, not the set,
# is what bounded the search at that tau: report the sides as unreliable rather
# than drawing endpoints too few points can support
logvar_bounds_tau_gate <- function(r, tau) {
  n_raw <- r$diagnostics$n_raw_feasible
  if (is.null(n_raw) || is.na(n_raw) ||
    n_raw >= LOGVAR_SEARCH_CONTROL$grid_floor) {
    return(r)
  }
  for (col in c("lower_status", "upper_status")) {
    bounded <- r$schema[[col]] == PAPER_ENDPOINT_STATUS[["bounded"]]
    r$schema[[col]][bounded] <- PAPER_ENDPOINT_STATUS[["unreliable"]]
  }
  cat(sprintf(
    "  thin lattice at tau = %.4g: %d raw feasible points; sides downgraded\n",
    tau, n_raw
  ))
  r
}

# Deterministic stand-in for "the knee reads as a knee". A cliff is one adjacent
# step that dwarfs the steps either side of it, which is what a branch switch
# looks like when the grid is too coarse to place a vertex inside it; a resolved
# knee spreads the same descent over several comparable steps. Reported per
# coefficient as the largest step measured against its larger neighbour, so the
# claim about shape is a number rather than a reading of the panel.
logvar_bounds_tau_steps <- function(rows) {
  bounded <- rows[rows$lower_status == PAPER_ENDPOINT_STATUS[["bounded"]], ]
  vapply(split(bounded, bounded$coef), function(s) {
    d <- abs(diff(s$lower[order(s$tau)]))
    if (length(d) < 3L) {
      return(NA_real_)
    }
    neighbour <- pmax(c(d[-1L], 0), c(0, d[-length(d)]))
    max(d / pmax(neighbour, .Machine$double.eps))
  }, numeric(1))
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
    b_tab <- mean_eq_bounds_tau[[paper_tau_key(tau)]]
    stopifnot(!is.null(b_tab))
    qs <- tau_quadratic_system(set_id_mean_eq$gamma, tau, set_id_mean_eq$moments)
    logvar_bounds_tau_gate(do.call(logvar_engine_set_at_tau, c(
      list(est, qs, b_tab, b_seed = entry$b_seed, extra_starts = extra, tau = tau),
      opts
    )), tau)
  }
  # an entry may opt out of the warm chain (warm_chain = FALSE): for a nonsmooth
  # map every warm arg is another derivative-free polish start on every endpoint,
  # so the chain is both the dominant cost and a second search protocol -- the
  # grid rows would be searched harder than the display rows drawn beside them
  res <- list()
  warm <- NULL
  for (tau in fig_tau_grid) {
    r <- run_tau(tau, warm)
    res[[paper_tau_key(tau)]] <- r
    warm <- if (identical(entry$warm_chain, FALSE)) {
      NULL
    } else {
      logvar_bounded_args(r$schema)
    }
  }
  grid_rows <- function() {
    do.call(rbind, lapply(res, function(r) {
      r$schema[c("tau", "coef", "lower", "upper", "lower_status", "upper_status")]
    }))
  }
  viol <- logvar_check_nesting(grid_rows())
  if (nrow(viol) > 0L) {
    for (tv in unique(viol$tau)) {
      k <- match(paper_tau_key(tv), names(res))
      near <- c(
        if (k > 1L) logvar_bounded_args(res[[k - 1L]]$schema),
        logvar_bounded_args(res[[k]]$schema),
        if (k < length(res)) logvar_bounded_args(res[[k + 1L]]$schema)
      )
      res[[k]] <- run_tau(tv, near)
    }
    viol <- logvar_check_nesting(grid_rows())
    for (i in seq_len(nrow(viol))) {
      v <- viol[i, ]
      k <- match(paper_tau_key(v$tau), names(res))
      col <- paste0(v$side, "_status")
      j <- match(v$coef, res[[k]]$schema$coef)
      res[[k]]$schema[[col]][j] <- "unreliable"
      cat(sprintf(
        "  nesting violation retained: %s %s side at tau = %.4g (gap %s); downgraded\n",
        v$coef,
        v$side,
        v$tau,
        paper_format_general(
          v$violation,
          PAPER_REPORTING_CONTROL$precision$console_significant
        )
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
  raw_counts <- vapply(res, function(r) {
    n <- r$diagnostics$n_raw_feasible
    if (is.null(n)) NA_integer_ else as.integer(n)
  }, integer(1))
  # all-missing means the engine stopped carrying the count, not that every tau
  # failed closed, and would silence the gate above without any symptom
  stopifnot(any(!is.na(raw_counts)))
  cat(
    "log-variance bounds-by-tau figure (", est$metadata$estimator, "): ",
    length(fig_tau_grid), " grid taus; min raw feasible ",
    min(raw_counts, na.rm = TRUE), "; crossings ",
    paste(vapply(res, function(r) r$n_cross, integer(1)), collapse = " "),
    "; cache hits ", opts$budget_state$counters[[LOGVAR_ENGINE_PHASES[["cache_hit"]]]],
    "; nesting downgrades ", nrow(viol),
    # a nonzero count means the news box no longer covers the set the polish can
    # reach, i.e. the box multistart has stopped keeping up with the grid walk
    "; box escapes ",
    sum(vapply(res, function(r) length(r$diagnostics$box_escape), integer(1))),
    "\n",
    sep = ""
  )
  steps <- logvar_bounds_tau_steps(grid_rows())
  cat(
    "  max lower-step dominance: ",
    paste(sprintf("%s %.1f", names(steps), steps), collapse = "; "), "\n",
    sep = ""
  )
}

for (fig_entry in logvar_bounds_tau_registry) logvar_bounds_tau_entry(fig_entry)

rm(
  fig_rows, fig_pcr, fig_fresh_id, fig_tau_grid,
  logvar_bounds_tau_entry, logvar_bounds_tau_gate, logvar_bounds_tau_steps,
  fig_entry
)
