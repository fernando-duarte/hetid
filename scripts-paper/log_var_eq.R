# Identified sets for the log-variance equation
#   log eps_{t+1}^2 = theta_0 + PC_{R,t}' theta_R + xi_{t+1}
# mapped over the mean equation's set-identified news coefficients: for each
# b_N in the joint identified set of set_id_mean_eq.R the fitted residual is
# eps_hat(b_N) = w1 - W2 b_N (the design coefficients are beta1(b_N) =
# beta1R - beta2R' b_N, so this is exact), theta_hat(b_N) is the OLS
# coefficient vector of log(eps_hat^2) on (1, PC_R), and the identified set
# of each coefficient at slack tau is the range of theta_hat over the joint
# set (feasible-grid scan plus SLSQP polish, log_var_eq_map.R). PC_R is the
# lagged asset-return PCs (asset_return_pcs.R), de-meaned over the estimation
# sample. Residual-zero crossings inside the joint set (log singularities,
# detected by the census unioned with the scan's sign tracker) make
# theta_hat_j diverge to -Inf where proj[j, t] > 0 and +Inf where
# proj[j, t] < 0: divergent sides are reported as infinite, finite sides are
# still computed, and any uncertifiable side fails the row closed as
# "unreliable".
# Run via run_all.R after set_id_mean_eq.R and asset_return_pcs.R.

source("scripts/utils/identification_utils.R")
source("scripts/utils/profile_bounds_core.R")
source("scripts/utils/profile_bounds.R")
source("scripts/utils/tau_star_utils.R")
source("scripts-paper/log_var_eq_map.R")

# grid resolution per b_N axis for the feasible-grid scan, and the feasible
# count below which the grid is densified once (a thin joint set can thread
# between lattice points of a box-tight grid); the size guard covers the
# densified retry, the largest grid ever built
logvar_grid_n <- 41L
logvar_grid_floor <- 100L
stopifnot((2L * logvar_grid_n - 1L)^ncol(set_id_mean_eq$w2) <= 1e6)

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

# identified-set intervals of every log-variance coefficient at one display
# slack: crossing census, then a joint-set feasible-grid scan (bounding box
# from the stored per-coefficient b_N intervals, densified once when the set
# threads the lattice, seeded with the tau = 0 point -- feasible at every
# tau by constraint nesting) and a two-start polish per finite side; the
# divergence bookkeeping unions the census with the scan's sign tracker.
# Fails closed: any upstream non-bounded b_N side, an unresolved census row,
# an empty feasible grid, a suspect polish, or a side with no accepted polish
logvar_set_at_tau <- function(tau, b_tab) {
  stopifnot(identical(b_tab$coef, colnames(w2_lv)))
  na_table <- function(status) {
    data.frame(
      coef = logvar_coefs, set_lower = NA_real_, set_upper = NA_real_,
      status = status, row.names = NULL
    )
  }
  out <- function(table, n_cross = NA_integer_, n_feasible = NA_integer_,
                  cross_qtr = NULL) {
    list(
      table = table, n_cross = n_cross, n_feasible = n_feasible,
      cross_qtr = cross_qtr
    )
  }
  if (any(b_tab$status != "bounded")) {
    # a certified-unbounded b_N axis is the firmer fact, so it wins here,
    # unlike the per-coefficient ladder below where unreliable fails closed
    status <- if (any(b_tab$status == "unbounded")) "unbounded" else "unreliable"
    return(out(na_table(status)))
  }
  qs <- tau_quadratic_system(set_id_mean_eq$gamma, tau, set_id_mean_eq$moments)

  census <- logvar_crossing_census(
    qs, b_tab$set_lower, b_tab$set_upper, w1_lv, w2_lv
  )
  if (length(census$unresolved) > 0L) {
    return(out(na_table("unreliable"), n_cross = length(census$cross)))
  }

  b_feas <- logvar_feasible_grid(qs, b_tab$set_lower, b_tab$set_upper, logvar_grid_n)
  if (nrow(b_feas) < logvar_grid_floor) {
    b_feas <- logvar_feasible_grid(
      qs, b_tab$set_lower, b_tab$set_upper, 2L * logvar_grid_n - 1L
    )
  }
  # an empty lattice fails closed before the tau = 0 seed is added: one
  # certified point cannot stand in for a set the grid could not resolve
  if (nrow(b_feas) == 0L) {
    return(out(na_table("unreliable"), n_cross = length(census$cross), n_feasible = 0L))
  }
  if (!anyNA(b_point)) {
    g_point <- vapply(seq_along(qs$A_i), function(i) {
      drop(t(b_point) %*% qs$A_i[[i]] %*% b_point) +
        sum(qs$b_i[[i]] * b_point) + qs$c_i[i]
    }, numeric(1))
    if (all(g_point <= 0)) b_feas <- rbind(b_feas, b_point)
  }
  scan <- logvar_grid_scan(b_feas, w1_lv, w2_lv, proj)

  # union of the two crossing detectors (exact on a connected set, biased
  # toward flagging on a disconnected one); a crossing at observation t
  # makes theta_hat_j diverge to -Inf where proj[j, t] > 0 and to +Inf
  # where proj[j, t] < 0
  cross_all <- sort(union(census$cross, scan$cross_grid))
  lower_unb <- apply(proj[, cross_all, drop = FALSE] > 0, 1, any)
  upper_unb <- apply(proj[, cross_all, drop = FALSE] < 0, 1, any)

  lower <- ifelse(lower_unb, -Inf, scan$min)
  upper <- ifelse(upper_unb, Inf, scan$max)
  unreliable <- rep(FALSE, length(logvar_coefs))
  for (j in seq_along(logvar_coefs)) {
    # blow-guard scale from the finite scan endpoints only (a divergent side
    # leaves an Inf in the scan), floored so a near-zero range still guards
    scan_j <- c(scan$min[j], scan$max[j])
    scale_j <- max(1, abs(scan_j[is.finite(scan_j)]))
    for (side in c("min", "max")) {
      if (if (side == "min") lower_unb[j] else upper_unb[j]) next
      starts <- list(if (side == "min") scan$arg_min[j, ] else scan$arg_max[j, ])
      if (!anyNA(b_point)) starts <- c(starts, list(b_point))
      accepted <- FALSE
      for (b_start in starts) {
        pol <- logvar_polish_bound(qs, side, b_start, scale_j, w1_lv, w2_lv, proj[j, ])
        if (pol$suspect) unreliable[j] <- TRUE
        if (is.null(pol$bound)) next
        accepted <- TRUE
        if (side == "min" && pol$bound < lower[j]) lower[j] <- pol$bound
        if (side == "max" && pol$bound > upper[j]) upper[j] <- pol$bound
      }
      if (!accepted) unreliable[j] <- TRUE
    }
  }
  status <- ifelse(
    unreliable, "unreliable",
    ifelse(lower_unb | upper_unb, "unbounded", "bounded")
  )
  out(
    data.frame(
      coef = logvar_coefs, set_lower = lower, set_upper = upper, status = status,
      row.names = NULL
    ),
    n_cross = length(cross_all), n_feasible = nrow(b_feas),
    cross_qtr = logvar_rows$qtr[cross_all]
  )
}

logvar_sets <- Map(
  logvar_set_at_tau,
  set_id_mean_eq$tau_display,
  lapply(set_id_mean_eq$set_tables, `[[`, "theta")
)
names(logvar_sets) <- names(set_id_mean_eq$set_tables)

logvar_table <- cbind(
  data.frame(
    coef = logvar_coefs,
    ols = unname(stats::coef(fit_logvar_ols)),
    point = unname(theta_point),
    row.names = NULL
  ),
  logvar_sets[[1]]$table[c("set_lower", "set_upper", "status")]
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
  # smallest absolute residual at the Lewbel point: how much slack there is
  # before some b_N in a neighborhood flips a residual sign
  min_abs_eps_point = if (anyNA(b_point)) {
    NA_real_
  } else {
    min(abs(drop(w1_lv - w2_lv %*% b_point)))
  }
)

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
  logvar_set_at_tau, logvar_sets, logvar_table
)
