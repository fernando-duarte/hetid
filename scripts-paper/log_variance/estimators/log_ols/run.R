# Identified sets for the log-variance equation
#   log eps_{t+1}^2 = theta_0 + PC_{R,t}' theta_R + xi_{t+1}
# mapped over the mean equation's set-identified news coefficients: for each
# b_N in the joint identified set of estimate_identified_set.R the fitted residual is
# eps_hat(b_N) = w1 - W2 b_N (the design coefficients are beta1(b_N) =
# beta1R - beta2R' b_N, so this is exact), theta_hat(b_N) is the OLS
# coefficient vector of log(eps_hat^2) on (1, PC_R), and the identified set
# of each coefficient at slack tau is the range of theta_hat over the joint
# set (feasible-grid scan plus SLSQP polish, residual_map.R). PC_R is the
# lagged asset-return PCs (build_asset_return_pcs.R), de-meaned over the estimation
# sample. Residual-zero crossings inside the joint set (log singularities,
# detected by the census unioned with the scan's sign tracker) make
# theta_hat_j diverge to -Inf where proj[j, t] > 0 and +Inf where
# proj[j, t] < 0: divergent sides are reported as infinite, finite sides are
# still computed, and any uncertifiable side fails the row closed as
# "unreliable".
# Run via run_pipeline.R after estimate_identified_set.R and build_asset_return_pcs.R.

paper_source_once(paper_path("support", "identification", "api.R"))
paper_source_once(paper_path("support", "identification", "profile_solver_core.R"))
paper_source_once(paper_path("support", "identification", "profile_bounds_api.R"))
paper_source_once(paper_path("support", "identification", "tau_star.R"))
paper_source_once(paper_path("log_variance", "core", "residual_map.R"))
paper_source_once(paper_path("log_variance", "engine", "api.R"))
paper_source_once(paper_path("log_variance", "estimators", "log_ols", "estimator.R"))
paper_source_once(paper_path(
  "log_variance", "estimators", "log_ols", "set_mapping.R"
))
paper_source_once(paper_path("log_variance", "diagnostics", "joint_null", "inputs.R"))

# grid resolution per b_N axis for the feasible-grid scan, and the feasible
# count below which the grid is densified once (a thin joint set can thread
# between lattice points of a box-tight grid); the size guard covers the
# densified retry, the largest grid ever built
logvar_grid_n <- LOGVAR_SEARCH_CONTROL$grid_n
logvar_grid_floor <- LOGVAR_SEARCH_CONTROL$grid_floor
stopifnot(
  (2L * logvar_grid_n - 1L)^ncol(set_id_mean_eq$w2) <=
    LOGVAR_SEARCH_CONTROL$logols_full_grid_safety_cap
)

# mean-equation sample rows with the lagged asset-return PCs available;
# `row` indexes into the stored aligned system pieces (join by qtr, never
# by position)
logvar_rows <- dplyr::inner_join(
  tibble::tibble(qtr = set_id_mean_eq$qtr, row = seq_along(set_id_mean_eq$qtr)),
  lag_asset_return_pc,
  by = "qtr"
) |>
  dplyr::arrange(qtr)
stopifnot(
  !anyDuplicated(set_id_mean_eq$qtr),
  !anyDuplicated(lag_asset_return_pc$qtr),
  nrow(logvar_rows) > 0L,
  !anyDuplicated(logvar_rows$row),
  # the b_N axis order every matrix product below assumes
  identical(colnames(set_id_mean_eq$w2), set_id_mean_eq$theta_table$coef)
)

w1_lv <- set_id_mean_eq$w1[logvar_rows$row]
w2_lv <- set_id_mean_eq$w2[logvar_rows$row, , drop = FALSE]
# de-meaned over the estimation sample, honoring the model's mean-zero PC_R;
# only theta_0's value depends on this convention
pcr <- scale(
  as.matrix(logvar_rows[value_cols(lag_asset_return_pc)]),
  center = TRUE, scale = FALSE
)
proj <- logvar_projection(pcr)
logvar_coefs <- rownames(proj)

# naive-analyst OLS column: the residuals of the exogenous-news OLS fit
# itself (its own jointly-estimated design coefficients, not the beta1(b_N)
# recovery), log-squared and regressed on the de-meaned PC_R; an lm fit so
# the table can attach Newey-West t statistics and an R^2
lv_ols <- log(stats::residuals(set_id_mean_eq$ols_fit)[logvar_rows$row]^2)
stopifnot(all(is.finite(lv_ols)))
fit_logvar_ols <- stats::lm(
  lv ~ .,
  data = cbind(data.frame(lv = lv_ols), as.data.frame(pcr))
)
stopifnot(identical(names(stats::coef(fit_logvar_ols)), logvar_coefs))

# closed-form Lewbel point column (tau = 0); NA when point identification
# failed upstream
b_point <- set_id_mean_eq$theta_table$point
theta_point <- if (anyNA(b_point)) {
  rep(NA_real_, length(logvar_coefs))
} else {
  logvar_theta_hat(b_point, w1_lv, w2_lv, proj)
}

# the log-OLS map packaged as the shared engine's first estimator object
# (closures over the frozen aligned sample; estimator.R)
logvar_est <- logvar_logols_estimator(
  w1_lv,
  w2_lv,
  proj,
  logvar_rows$qtr,
  pcr,
  control = LOGVAR_LOGOLS_CONTROL
)

# identified-set intervals of every log-variance coefficient at one display
# slack, through the shared engine in the explicit, complete benchmark
# configuration: scan_grid fast path, no extra starts, no coarsening, no
# budget, no cache, and no cold-start check (the closed-form map needs no
# replication). Census, feasible grid, tau = 0 seed injection, two-start
# polish, divergence bookkeeping, and the fail-closed ladder all run inside
# the engine exactly as the old inline closure did; crossing indices map
# back to sample quarters here
logvar_sets <- logvar_logols_sets(
  logvar_est,
  set_id_mean_eq,
  b_point,
  logvar_grid_n,
  logvar_grid_floor,
  logvar_rows$qtr,
  colnames(w2_lv)
)

logvar_table <- cbind(
  data.frame(
    coef = logvar_coefs,
    ols = unname(stats::coef(fit_logvar_ols)),
    point = unname(theta_point),
    row.names = NULL
  ),
  logvar_sets[[paper_tau_key(set_id_mean_eq$tau_baseline)]]$table[
    c("set_lower", "set_upper", "status")
  ]
)

log_var_eq <- list(
  sample = list(n = nrow(logvar_rows), span = range(logvar_rows$qtr)),
  coefs = logvar_coefs,
  table = logvar_table,
  sets = lapply(logvar_sets, `[[`, "table"),
  n_cross = vapply(logvar_sets, `[[`, integer(1), "n_cross"),
  n_feasible = vapply(logvar_sets, `[[`, integer(1), "n_feasible"),
  # which sample quarters drive the divergence at each slack
  cross_qtr = lapply(logvar_sets, `[[`, "cross_qtr"),
  fit_ols = fit_logvar_ols,
  grid_n = logvar_grid_n,
  tau_baseline = set_id_mean_eq$tau_baseline,
  # smallest absolute residual at the Lewbel point: how much slack there is
  # before some b_N in a neighborhood flips a residual sign
  min_abs_eps_point = if (anyNA(b_point)) {
    NA_real_
  } else {
    min(abs(drop(w1_lv - w2_lv %*% b_point)))
  },
  # additive engine-era fields: the per-tau side-specific schema tables, the
  # estimator object itself (closures + metadata, consumed by the figure),
  # the tau = 0 seed, and the sample guard
  schema = lapply(logvar_sets, `[[`, "schema"),
  estimator = logvar_est,
  b_seed = b_point,
  sample_id = logvar_est$metadata$sample_id
)
# frozen inputs and the explicit post-trim sample contract: the one
# preparation path every later estimator consumes (additive only)
log_var_eq$inputs <- list(
  w1 = w1_lv, w2 = w2_lv, pcr = pcr, qtr = logvar_rows$qtr
)
# joint-null diagnostic seam: attach the qtr-aligned naive-fit residual reference
# eps_ref (additive only; supplies the crossing-stability median(abs(eps_ref)))
log_var_eq$inputs <- logvar_joint_null_extend_inputs(
  log_var_eq$inputs,
  stats::residuals(set_id_mean_eq$ols_fit)[logvar_rows$row],
  logvar_rows$qtr
)
log_var_eq$sample_contract <- list(
  qtr = logvar_rows$qtr, n = nrow(logvar_rows), pc_names = colnames(pcr),
  sample_id = log_var_eq$sample_id
)
# the bounds-by-tau figure renders one estimator-stamped SVG per registry
# entry; the benchmark is entry one
logvar_bounds_tau_registry <- list(list(
  estimator = logvar_est, schema = log_var_eq$schema, sets = log_var_eq$sets,
  b_seed = b_point, engine_opts = list(),
  output_path = logvar_bounds_tau_path(logvar_est$metadata)
))

cat(
  "log-variance equation: N =", log_var_eq$sample$n,
  "over", format(log_var_eq$sample$span[1]), "to",
  format(log_var_eq$sample$span[2]),
  "\n  crossings by tau:",
  paste(names(log_var_eq$n_cross), log_var_eq$n_cross, sep = "=", collapse = " "),
  "\n  min |eps_hat| at the tau = 0 point:",
  signif(log_var_eq$min_abs_eps_point, 3), "\n"
)
print(log_var_eq$table, digits = 3)

rm(
  logvar_grid_n, logvar_grid_floor, logvar_rows, w1_lv, w2_lv, pcr, proj,
  logvar_coefs, lv_ols, fit_logvar_ols, b_point, theta_point,
  logvar_est, logvar_sets, logvar_table
)
